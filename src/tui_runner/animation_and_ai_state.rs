use crate::{
    panic_handling::report_panic,
    pieces::Player,
    state::HiveGameState,
    tui_runner::{
        tui_animations::loader,
        tui_settings::{AIMoves, Settings},
    },
    worker::{
        ai_worker::{AIEndpoint, AIResult, AIStart},
        WorkerResult,
    },
    FatalError,
};

use super::tui_animations::Animation;

pub struct CameraMove {
    current_step: usize,
    max_steps: usize,
    from: (f64, f64),
    to: (f64, f64),
}

impl CameraMove {
    pub fn build(steps: usize, from: (f64, f64), to: (f64, f64)) -> Self {
        Self {
            current_step: 0,
            max_steps: steps,
            from,
            to,
        }
    }

    pub fn make_step(&mut self) -> Option<(f64, f64)> {
        if self.current_step >= self.max_steps {
            return None;
        }

        let pos_from_frac = |fraction| {
            let progress = 2.0
                * if fraction <= 0.5 {
                    fraction * fraction
                } else {
                    0.5 - (1.0 - fraction) * (1.0 - fraction)
                };
            (
                (1.0 - progress) * self.from.0 + progress * self.to.0,
                (1.0 - progress) * self.from.1 + progress * self.to.1,
            )
        };

        let frac_old = self.current_step as f64 / self.max_steps as f64;
        self.current_step += 1;
        let frac = self.current_step as f64 / self.max_steps as f64;
        let (x_old, y_old) = pos_from_frac(frac_old);
        let (x_new, y_new) = pos_from_frac(frac);
        Some((x_new - x_old, y_new - y_old))
    }
}

#[derive(Default)]
pub struct AnimationState {
    animation: Option<Animation>,
    count: usize,
    center: Option<(f64, f64)>,
}

pub struct AnimationStateSetter<'a> {
    pub animation: &'a mut Option<Animation>,
    pub count: &'a mut usize,
    pub center: &'a mut Option<(f64, f64)>,
}

impl AnimationStateSetter<'_> {
    pub fn set_animation(self, a: Animation, center: Option<(f64, f64)>) {
        *self.animation = Some(a);
        *self.count += 1;
        *self.center = center;
    }
}

impl AnimationState {
    pub fn animation(&self) -> Option<&Animation> {
        self.animation.as_ref()
    }

    pub fn center(&self) -> Option<(f64, f64)> {
        self.center
    }

    pub fn runs(&self) -> bool {
        self.animation.is_some()
    }

    pub fn next_step(&mut self) {
        if let Some(animation) = self.animation.as_mut() {
            if animation.is_finished() {
                self.stop();
            } else {
                animation.next_step();
            }
        }
    }

    pub fn try_set(&mut self) -> Option<AnimationStateSetter<'_>> {
        match self.animation {
            Some(_) => None,
            None => Some(AnimationStateSetter {
                animation: &mut self.animation,
                count: &mut self.count,
                center: &mut self.center,
            }),
        }
    }

    pub fn stop(&mut self) {
        self.animation = None;
        self.center = None;
    }

    pub fn reset(&mut self) {
        self.stop();
        self.count = 0;
    }

    pub fn reset_count(&mut self) {
        self.count = 0;
    }
}

pub struct AIState {
    endpoint: AIEndpoint,
    current_player: Player,
    is_started: bool,
    // TODO: update in case of menu changes?!
    should_use_ai: bool,
    should_show_animation: bool,
    result: Option<AIResult>,
    animation_progress: usize,
}

impl AIState {
    const AI_DELAY: usize = 40;

    pub fn new(endpoint: AIEndpoint) -> Self {
        Self {
            endpoint,
            current_player: Player::White,
            is_started: false,
            should_use_ai: false,
            should_show_animation: false,
            result: None,
            animation_progress: 0,
        }
    }

    pub fn reset(&mut self) {
        self.endpoint.cancel();
        self.is_started = false;
        self.result = None;
        self.animation_progress = 0;
    }

    pub fn update<F>(
        &mut self,
        state: &HiveGameState,
        settings: &Settings,
        player: Player,
        animation: &mut AnimationState,
        postprocess_fn: F,
    ) -> Result<(), FatalError>
    where
        F: Fn(&mut AIResult, &Settings),
    {
        // TODO: try to disentangle from TUI specific logic?
        // TODO: pause when undo
        assert!(self.current_player == player || !self.is_started);
        if !self.is_started {
            assert!(self.result.is_none() && self.animation_progress == 0);
            let level = if settings.is_ai(player) {
                settings.player_type(player).into_ai_level()
            } else {
                settings.ai_assistant
            };
            self.endpoint
                .send_overwrite(AIStart(level.as_difficulty(), Box::new(state.clone())));
            self.current_player = player;
            self.should_use_ai = settings.is_ai(player) && settings.ai_moves == AIMoves::Automatic;
            self.should_show_animation = settings.is_ai(player);
            self.is_started = true;
        }

        {
            match self.endpoint.get_msg() {
                Some(WorkerResult::Msg(mut result)) => {
                    postprocess_fn(result.as_mut(), settings);
                    self.result = Some(*result);
                }
                Some(WorkerResult::Killed(msg, trace)) => {
                    report_panic(Some(msg), trace);
                    return Err(FatalError::PanicOccured);
                }
                None => (),
            }
        }
        if self.should_show_animation {
            if self.animation_progress > 0 || !animation.runs() {
                self.animation_progress += 1;
            }
            if self.animation_progress < Self::AI_DELAY || self.result.is_none() {
                if let Some(s) = animation.try_set() {
                    s.set_animation(Animation::new(loader(settings, 30)), None)
                }
            }
        }
        Ok(())
    }

    pub fn should_use_ai(&self) -> bool {
        self.should_use_ai
    }

    pub fn use_ai(&mut self, settings: &Settings) {
        if settings.is_ai(self.current_player) {
            self.should_use_ai = true;
            self.should_show_animation = true;
        }
    }

    pub fn dont_use_ai(&mut self) {
        self.should_use_ai = false;
        self.should_show_animation = false;
        self.animation_progress = 0;
    }

    pub fn result(&self) -> Option<&AIResult> {
        if self.animation_progress >= Self::AI_DELAY {
            self.result.as_ref()
        } else {
            None
        }
    }

    pub fn actual_result(&self) -> Option<&AIResult> {
        self.result.as_ref()
    }

    pub fn animation_has_started(&self) -> bool {
        self.animation_progress > 0
    }
}

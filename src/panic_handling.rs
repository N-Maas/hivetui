use std::{backtrace::Backtrace, cell::RefCell, panic};

thread_local! {
    static MSG: RefCell<Option<String>> = RefCell::new(None);
    static BACKTRACE: RefCell<Option<Backtrace>> = RefCell::new(None);
}

pub fn setup_panic_reporting() {
    panic::set_hook(Box::new(|info| {
        let payload = info.payload();
        let msg = payload.downcast_ref::<String>().cloned().or_else(|| {
            payload
                .downcast_ref::<&'static str>()
                .map(|s| s.to_string())
        });
        msg.map(|value| {
            MSG.with(|b| b.borrow_mut().replace(value));
        });
        let trace = Backtrace::force_capture();
        BACKTRACE.with(move |b| b.borrow_mut().replace(trace));
    }));
}

pub fn get_panic_data() -> (String, String) {
    let msg = MSG
        .with(|b| b.borrow_mut().take())
        .map_or(String::new(), |m| m + "\n");
    let trace = BACKTRACE
        .with(|b| b.borrow_mut().take())
        .map_or(String::new(), |b| b.to_string());
    (msg, trace)
}

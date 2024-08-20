use std::{backtrace::Backtrace, cell::RefCell, panic};

thread_local! {
    static MSG: RefCell<Option<String>> = RefCell::new(None);
    static BACKTRACE: RefCell<Option<String>> = RefCell::new(None);
}

pub fn setup_panic_reporting() {
    panic::set_hook(Box::new(|info| {
        let trace = Backtrace::force_capture();
        let payload = info.payload();
        let msg = payload.downcast_ref::<String>().cloned().or_else(|| {
            payload
                .downcast_ref::<&'static str>()
                .map(|s| s.to_string())
        });
        report_panic(msg, trace.to_string());
    }));
}

pub fn report_panic(msg: Option<String>, trace: String) {
    if let Some(value) = msg {
        MSG.with(|b| b.borrow_mut().replace(value));
    }
    BACKTRACE.with(move |b| b.borrow_mut().replace(trace));
}

pub fn get_panic_data() -> (String, String) {
    let msg = MSG.with(|b| b.borrow_mut().take());
    let trace = BACKTRACE.with(|b| b.borrow_mut().take());
    (msg.unwrap_or_default(), trace.unwrap_or_default())
}

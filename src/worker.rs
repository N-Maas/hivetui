use crate::panic_handling::{get_panic_data, setup_panic_reporting};
use std::{
    panic::{catch_unwind, UnwindSafe},
    sync::{Arc, Mutex},
    thread,
};

pub mod ai_worker;
pub mod io_worker;

#[derive(Debug)]
enum MessageForWorker<T> {
    Msg(T),
    Cancel,
}

#[derive(Debug)]
pub enum WorkerResult<T> {
    Msg(T),
    Killed(String, String),
}

struct Exchange<M, W> {
    for_master: Option<WorkerResult<M>>,
    for_worker: Option<MessageForWorker<W>>,
}

impl<M, W> Default for Exchange<M, W> {
    fn default() -> Self {
        Self {
            for_master: None,
            for_worker: None,
        }
    }
}

pub struct MasterEndpoint<M, W>(Arc<Mutex<Exchange<M, W>>>);

impl<M, W> MasterEndpoint<M, W> {
    pub fn get_msg(&self) -> Option<WorkerResult<M>> {
        self.0.lock().unwrap().for_master.take()
    }

    pub fn cancel(&self) {
        self.0.lock().unwrap().for_worker = Some(MessageForWorker::Cancel);
    }

    pub fn send(&self, message: W) {
        self.0.lock().unwrap().for_worker = Some(MessageForWorker::Msg(message));
    }
}

struct WorkerEndpoint<M, W>(Arc<Mutex<Exchange<M, W>>>);

impl<M, W> WorkerEndpoint<M, W> {
    fn get_new_msg(&self) -> Option<W> {
        let mut exchange = self.0.lock().unwrap();
        exchange.for_worker.take().and_then(|m| match m {
            MessageForWorker::Msg(msg) => Some(msg),
            MessageForWorker::Cancel => None,
        })
    }

    fn has_msg(&self) -> bool {
        self.0.lock().unwrap().for_worker.is_some()
    }

    fn send_if_no_msg(&self, message: M) {
        let mut exchange = self.0.lock().unwrap();
        let has_msg = exchange.for_worker.is_some();
        if !has_msg {
            exchange.for_master = Some(WorkerResult::Msg(message));
        }
    }

    fn killed(&self, msg: String, trace: String) {
        self.0.lock().unwrap().for_master = Some(WorkerResult::Killed(msg, trace));
    }
}

fn setup_exchange<M, W>() -> (MasterEndpoint<M, W>, WorkerEndpoint<M, W>) {
    let exchange = Arc::new(Mutex::new(Exchange::default()));
    (MasterEndpoint(exchange.clone()), WorkerEndpoint(exchange))
}

fn start_worker_thread<M, W, F>(worker_loop: F) -> MasterEndpoint<M, W>
where
    M: Send + 'static,
    W: Send + 'static,
    F: FnOnce(&WorkerEndpoint<M, W>) + UnwindSafe + Send + 'static,
{
    let (master_endpoint, endpoint) = setup_exchange();
    thread::spawn(move || {
        setup_panic_reporting();

        let result = catch_unwind(|| worker_loop(&endpoint));
        if result.is_err() {
            let (msg, trace) = get_panic_data();
            endpoint.killed(msg, trace);
        }
    });
    master_endpoint
}

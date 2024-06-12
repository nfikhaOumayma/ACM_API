/*
 * 
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * The Class JournalEntryWorkflowStepException.
 */
public class JournalEntryWorkflowStepException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3901672205432870043L;
	/** The message. */
	private String message;
	
	/**
	 * Instantiates a new journal entry workflow step exception.
	 *
	 * @param message the message
	 */
	public JournalEntryWorkflowStepException(String message) {
		this.message = message;
	}
	
	/**
	 * Instantiates a new journal entry workflow step exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public JournalEntryWorkflowStepException(String exception, String message) {
		super(exception);
		this.message = message;
	}
	
	/**
	 * Instantiates a new journal entry workflow step exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public JournalEntryWorkflowStepException(ExceptionResponseMessage exceptionResponseMessage) {
		super(exceptionResponseMessage);
	}
	
	/**
	 * Instantiates a new journal entry workflow step exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public JournalEntryWorkflowStepException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {
		super(exceptionResponseMessage);
		this.message = message;
	}
	
	/**
	 * Instantiates a new journal entry workflow step exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 * @param exceptionResponseMessage the exception response message
	 */
	public JournalEntryWorkflowStepException(String exception, String message, ExceptionResponseMessage exceptionResponseMessage) {
		super(exception, exceptionResponseMessage);
		this.message = message;
	}
	
	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}
	
	/**
	 * Sets the message.
	 *
	 * @param message the new message
	 */
	public void setMessage(String message) {
		this.message = message;
	}
}

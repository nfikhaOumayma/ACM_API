package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link RequestAlreadyExistException} class.
 *
 * @author ManelLamloum
 * @since 0.1.0
 */
public class RequestAlreadyExistException extends CustomException {

	private static final long serialVersionUID = -292697726451570384L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param message the message
	 */
	public RequestAlreadyExistException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public RequestAlreadyExistException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public RequestAlreadyExistException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public RequestAlreadyExistException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public RequestAlreadyExistException(String exception,
			ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exception, exceptionResponseMessage);
		this.message = message;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {

		return message;
	}

	/**
	 * Sets message.
	 *
	 * @param message the message
	 */
	public void setMessage(String message) {

		this.message = message;
	}
}

package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link NationalIdNotFoundException } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class NationalIdNotFoundException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -643732408070172931L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new national id not found exception.
	 *
	 * @param message the message
	 */
	public NationalIdNotFoundException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new national id not found exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public NationalIdNotFoundException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new national id not found exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public NationalIdNotFoundException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new national id not found exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public NationalIdNotFoundException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new national id not found exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public NationalIdNotFoundException(String exception,
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
	 * Sets the message.
	 *
	 * @param message the new message
	 */
	public void setMessage(String message) {

		this.message = message;
	}
}

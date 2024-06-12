/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link GEDException} Ged exception.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public class GEDException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5087482291046679005L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new Ged exception.
	 *
	 * @param message the message
	 */
	public GEDException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new Ged exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public GEDException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * instanciate exceptionResponseMessage {@link ExceptionResponseMessage}.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public GEDException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new Ged exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public GEDException(ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new Ged exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public GEDException(String exception, ExceptionResponseMessage exceptionResponseMessage,
			String message) {

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

	/**
	 * size depassed exception for GED uploading file.
	 */
	public static class SizeFileException extends GEDException {

		/** The Constant serialVersionUID. */
		private static final long serialVersionUID = -706210961440954220L;

		/**
		 * Default constructor for static exception.
		 *
		 * @param message the error message
		 */
		public SizeFileException(String message) {

			super(message);
		}
	}

	/**
	 * file existing exception.
	 */
	public static class FileExisting extends GEDException {

		/** The Constant serialVersionUID. */
		private static final long serialVersionUID = 9211109737264234751L;

		/**
		 * default exception file existing.
		 *
		 * @param message the error message
		 */
		public FileExisting(String message) {

			super(message);
		}

		/**
		 * file existing exception.
		 *
		 * @param exceptionResponseMessage the error message
		 */
		public FileExisting(ExceptionResponseMessage exceptionResponseMessage) {

			super(exceptionResponseMessage);
		}
	}
}

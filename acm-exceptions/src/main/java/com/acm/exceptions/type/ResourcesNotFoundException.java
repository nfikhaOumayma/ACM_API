/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link ResourcesNotFoundException} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class ResourcesNotFoundException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6187290919874963989L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new custom not found exception.
	 */
	public ResourcesNotFoundException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new custom not found exception.
	 *
	 * @param message the message
	 */
	public ResourcesNotFoundException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public ResourcesNotFoundException(String exception, String message) {

		super(exception);
		this.setMessage(message);
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
	 * @param message the message to set
	 */
	public void setMessage(String message) {

		this.message = message;
	}

}

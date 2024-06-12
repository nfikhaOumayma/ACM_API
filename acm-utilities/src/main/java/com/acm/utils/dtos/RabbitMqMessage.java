package com.acm.utils.dtos;

/**
 * The Class RabbitMqMessage.
 */
public class RabbitMqMessage {

	/** The message. */
	private String message;

	/** The index. */
	private String index;

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

	/**
	 * Gets the index.
	 *
	 * @return the index
	 */
	public String getIndex() {

		return index;
	}

	/**
	 * Sets the index.
	 *
	 * @param index the new index
	 */
	public void setIndex(String index) {

		this.index = index;
	}

}

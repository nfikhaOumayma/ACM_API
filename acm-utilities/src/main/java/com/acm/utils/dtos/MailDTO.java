/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link MailDTO} class to define mail informations to use while sending mail.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class MailDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4247558001988631282L;

	/** The from. */
	private String from;

	/** The to. */
	private String to;

	/** The subject. */
	private String subject;

	/** The content. */
	private String content;

	/** The cc. */
	private String cc;

	/**
	 * Instantiates a new mail DTO.
	 */
	public MailDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new mail DTO.
	 *
	 * @param from the from
	 * @param to the to
	 * @param subject the subject
	 * @param content the content
	 */
	public MailDTO(String from, String to, String subject, String content) {

		this.from = from;
		this.to = to;
		this.subject = subject;
		this.content = content;
	}

	/**
	 * Gets the from.
	 *
	 * @return the from
	 */
	public String getFrom() {

		return from;
	}

	/**
	 * Sets the from.
	 *
	 * @param from the new from
	 */
	public void setFrom(String from) {

		this.from = from;
	}

	/**
	 * Gets the to.
	 *
	 * @return the to
	 */
	public String getTo() {

		return to;
	}

	/**
	 * Sets the to.
	 *
	 * @param to the new to
	 */
	public void setTo(String to) {

		this.to = to;
	}

	/**
	 * Gets the subject.
	 *
	 * @return the subject
	 */
	public String getSubject() {

		return subject;
	}

	/**
	 * Sets the subject.
	 *
	 * @param subject the new subject
	 */
	public void setSubject(String subject) {

		this.subject = subject;
	}

	/**
	 * Gets the content.
	 *
	 * @return the content
	 */
	public String getContent() {

		return content;
	}

	/**
	 * Sets the content.
	 *
	 * @param content the new content
	 */
	public void setContent(String content) {

		this.content = content;
	}

	/**
	 * Gets the cc.
	 *
	 * @return the cc
	 */
	public String getCc() {

		return cc;
	}

	/**
	 * Sets the cc.
	 *
	 * @param cc the new cc
	 */
	public void setCc(String cc) {

		this.cc = cc;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Mail{" + "from='" + from + '\'' + ", to='" + to + '\'' + ", subject='" + subject
				+ '\'' + ", content='" + content + '\'' + '}';
	}
}

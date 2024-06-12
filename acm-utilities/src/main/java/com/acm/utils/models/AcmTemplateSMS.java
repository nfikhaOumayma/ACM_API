/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class AcmTemplateSMS.
 */
@Entity
@Table(name = "ACM_TEMPLATE_SMS")
public class AcmTemplateSMS {

	/** The id acm template SMS. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_TEMPLATE_SMS", unique = true, nullable = false)
	private Long idAcmTemplateSMS;

	/** The code SMS event. */
	@Column(name = "SMS_EVENT_CODE")
	private String codeSMSEvent;

	/** The message body. */
	@Column(name = "MESSAGE_BODY")
	private String messageBody;

	/** The Date envoi. */
	@Column(name = "DATE_ENVOI")
	private String DateEnvoi;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the id acm template SMS.
	 *
	 * @return the id acm template SMS
	 */
	public Long getIdAcmTemplateSMS() {

		return idAcmTemplateSMS;
	}

	/**
	 * Sets the id acm template SMS.
	 *
	 * @param idAcmTemplateSMS the new id acm template SMS
	 */
	public void setIdAcmTemplateSMS(Long idAcmTemplateSMS) {

		this.idAcmTemplateSMS = idAcmTemplateSMS;
	}

	/**
	 * Instantiates a new acm template SMS.
	 */
	public AcmTemplateSMS() {

		super();
	}

	/**
	 * Gets the code SMS event.
	 *
	 * @return the code SMS event
	 */
	public String getCodeSMSEvent() {

		return codeSMSEvent;
	}

	/**
	 * Sets the code SMS event.
	 *
	 * @param codeSMSEvent the new code SMS event
	 */
	public void setCodeSMSEvent(String codeSMSEvent) {

		this.codeSMSEvent = codeSMSEvent;
	}

	/**
	 * Gets the message body.
	 *
	 * @return the message body
	 */
	public String getMessageBody() {

		return messageBody;
	}

	/**
	 * Sets the message body.
	 *
	 * @param messageBody the new message body
	 */
	public void setMessageBody(String messageBody) {

		this.messageBody = messageBody;
	}

	/**
	 * Gets the date envoi.
	 *
	 * @return the date envoi
	 */
	public String getDateEnvoi() {

		return DateEnvoi;
	}

	/**
	 * Sets the date envoi.
	 *
	 * @param dateEnvoi the new date envoi
	 */
	public void setDateEnvoi(String dateEnvoi) {

		DateEnvoi = dateEnvoi;
	}

	/**
	 * Instantiates a new acm template SMS.
	 *
	 * @param idAcmTemplateSMS the id acm template SMS
	 * @param codeSMSEvent the code SMS event
	 * @param messageBody the message body
	 * @param dateEnvoi the date envoi
	 */
	public AcmTemplateSMS(Long idAcmTemplateSMS, String codeSMSEvent, String messageBody,
			String dateEnvoi) {

		super();
		this.idAcmTemplateSMS = idAcmTemplateSMS;
		this.codeSMSEvent = codeSMSEvent;
		this.messageBody = messageBody;
		DateEnvoi = dateEnvoi;

	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link TransversHistorique} class.
 *
 * @author MoezMhiri
 * @since 1.0.12
 */
@Entity
@Table(name = "ACM_TRANSVERS_HISTORY")
public class TransversHistorique extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1752442082745594731L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_TRANSVERS_HISTORY", unique = true, nullable = false)
	private Long id;

	/** The object value. */
	@Column(name = "OBJECT_VALUE")
	private String objectValue;

	/** The methode => {@link transversHistorique} POST - GET - PUT. */
	@Column(name = "METHODE")
	private String methode;

	/** The uri. */
	@Column(name = "URI")
	private String uri;

	/** The status. */
	@Column(name = "REPONSE_STATUS")
	private String status;

	/** The request value. */
	@Column(name = "REQUEST_VALUE")
	private String requestValue;

	/** The response value. */
	@Column(name = "RESPONSE_VALUE")
	private String responseValue;

	/**
	 * Instantiates a new third party historique.
	 */
	public TransversHistorique() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new transvers historique.
	 *
	 * @param objectValue the object value
	 * @param methode the methode
	 * @param uri the uri
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 */
	public TransversHistorique(String objectValue, String methode, String uri, String status,
			String requestValue, String responseValue) {

		this.objectValue = objectValue;
		this.methode = methode;
		this.uri = uri;
		this.status = status;
		this.requestValue = requestValue;
		this.responseValue = responseValue;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the object value.
	 *
	 * @return the objectValue
	 */
	public String getObjectValue() {

		return objectValue;
	}

	/**
	 * Sets the object value.
	 *
	 * @param objectValue the objectValue to set
	 */
	public void setObjectValue(String objectValue) {

		this.objectValue = objectValue;
	}

	/**
	 * Gets the methode.
	 *
	 * @return the methode
	 */
	public String getMethode() {

		return methode;
	}

	/**
	 * Sets the methode.
	 *
	 * @param methode the methode to set
	 */
	public void setMethode(String methode) {

		this.methode = methode;
	}

	/**
	 * Gets the uri.
	 *
	 * @return the uri
	 */
	public String getUri() {

		return uri;
	}

	/**
	 * Sets the uri.
	 *
	 * @param uri the uri to set
	 */
	public void setUri(String uri) {

		this.uri = uri;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the request value.
	 *
	 * @return the requestValue
	 */
	public String getRequestValue() {

		return requestValue;
	}

	/**
	 * Sets the request value.
	 *
	 * @param requestValue the requestValue to set
	 */
	public void setRequestValue(String requestValue) {

		this.requestValue = requestValue;
	}

	/**
	 * Gets the response value.
	 *
	 * @return the responseValue
	 */
	public String getResponseValue() {

		return responseValue;
	}

	/**
	 * Sets the response value.
	 *
	 * @param responseValue the responseValue to set
	 */
	public void setResponseValue(String responseValue) {

		this.responseValue = responseValue;
	}

}

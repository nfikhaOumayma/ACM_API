/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * The Class TransversHistoriqueDTO.
 * 
 * @author MOEZ
 * @since 1.0.12
 */
public class TransversHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4169462163879804774L;

	/** The id. */
	private Long id;

	/** The object value. */
	private String objectValue;
	/** The methode => {@link transversHistorique} POST - GET - PUT. */
	private String methode;

	/** The uri. */
	private String uri;

	/** The status. */
	private String status;

	/** The request value. */
	private String requestValue;

	/** The response value. */
	private String responseValue;

	/** The date insertion. */
	private Date dateInsertion;

	/** The insert by. */
	private String insertBy;

	/**
	 * Instantiates a new transvers historique DTO.
	 *
	 * @param objectValue the object value
	 * @param methode the methode
	 * @param uri the uri
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 */
	public TransversHistoriqueDTO(String objectValue, String methode, String uri, String status,
			String requestValue, String responseValue) {

		this.objectValue = objectValue;
		this.methode = methode;
		this.uri = uri;
		this.status = status;
		this.requestValue = requestValue;
		this.responseValue = responseValue;

	}

	/**
	 * Instantiates a new third party historique DTO.
	 */
	public TransversHistoriqueDTO() {

		/*
		 * EMPTY
		 */
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

	/**
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "TransversHistoriqueDTO [id=" + id + ", objectValue=" + objectValue + ", methode="
				+ methode + ", uri=" + uri + ", status=" + status + ", requestValue=" + requestValue
				+ ", responseValue=" + responseValue + ", dateInsertion=" + dateInsertion
				+ ", insertBy=" + insertBy + "]";
	}

}

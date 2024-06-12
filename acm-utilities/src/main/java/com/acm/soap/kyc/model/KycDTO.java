/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.kyc.model;

import java.io.Serializable;

import com.acm.utils.dtos.GenericDTO;

/**
 * {@link KycDTO} class.
 *
 * @author yesser.somai
 * @since 1.0.15
 */
public class KycDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8110789880080071100L;

	/** The national ID. */
	private String nationalID;

	/** The xml request. */
	private String xmlRequest;

	/** The xml response. */
	private PersonDetails xmlResponse;

	/**
	 * Instantiates a new kyc DTO.
	 */
	public KycDTO() {

	}

	/**
	 * Gets the national ID.
	 *
	 * @return the nationalID
	 */
	public String getNationalID() {

		return nationalID;
	}

	/**
	 * Sets the national ID.
	 *
	 * @param nationalID the nationalID to set
	 */
	public void setNationalID(String nationalID) {

		this.nationalID = nationalID;
	}

	/**
	 * Gets the xml request.
	 *
	 * @return the xmlRequest
	 */
	public String getXmlRequest() {

		return xmlRequest;
	}

	/**
	 * Sets the xml request.
	 *
	 * @param xmlRequest the xmlRequest to set
	 */
	public void setXmlRequest(String xmlRequest) {

		this.xmlRequest = xmlRequest;
	}

	/**
	 * Gets the xml response.
	 *
	 * @return the xmlResponse
	 */
	public PersonDetails getXmlResponse() {

		return xmlResponse;
	}

	/**
	 * Sets the xml response.
	 *
	 * @param xmlResponse the xmlResponse to set
	 */
	public void setXmlResponse(PersonDetails xmlResponse) {

		this.xmlResponse = xmlResponse;
	}

}

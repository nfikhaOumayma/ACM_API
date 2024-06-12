/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.soap.kyc.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Classe Java pour CompanyInformationResponse complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="CompanyInformationResponse"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="companies" type="{http://main}ArrayOfCompany"/&gt;
 *         &lt;element name="requestCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="responseCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="responseDesc" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CompanyInformationResponse",
		propOrder = {"companies", "requestCode", "responseCode", "responseDesc"})
public class CompanyInformationResponse {

	/** The companies. */
	@XmlElement(required = true, nillable = true)
	protected ArrayOfCompany companies;

	/** The request code. */
	@XmlElement(required = true, nillable = true)
	protected String requestCode;

	/** The response code. */
	@XmlElement(required = true, nillable = true)
	protected String responseCode;

	/** The response desc. */
	@XmlElement(required = true, nillable = true)
	protected String responseDesc;

	/**
	 * Obtient la valeur de la propriété companies.
	 * 
	 * @return possible object is {@link ArrayOfCompany }
	 */
	public ArrayOfCompany getCompanies() {

		return companies;
	}

	/**
	 * Définit la valeur de la propriété companies.
	 * 
	 * @param value allowed object is {@link ArrayOfCompany }
	 */
	public void setCompanies(ArrayOfCompany value) {

		this.companies = value;
	}

	/**
	 * Obtient la valeur de la propriété requestCode.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getRequestCode() {

		return requestCode;
	}

	/**
	 * Définit la valeur de la propriété requestCode.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setRequestCode(String value) {

		this.requestCode = value;
	}

	/**
	 * Obtient la valeur de la propriété responseCode.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getResponseCode() {

		return responseCode;
	}

	/**
	 * Définit la valeur de la propriété responseCode.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setResponseCode(String value) {

		this.responseCode = value;
	}

	/**
	 * Obtient la valeur de la propriété responseDesc.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getResponseDesc() {

		return responseDesc;
	}

	/**
	 * Définit la valeur de la propriété responseDesc.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setResponseDesc(String value) {

		this.responseDesc = value;
	}

}

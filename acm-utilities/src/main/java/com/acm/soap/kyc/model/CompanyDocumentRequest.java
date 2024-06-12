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
 * Classe Java pour CompanyDocumentRequest complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="CompanyDocumentRequest"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="craNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="password" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="requestCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="userCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CompanyDocumentRequest",
		propOrder = {"craNumber", "password", "requestCode", "userCode"})
public class CompanyDocumentRequest {

	/** The cra number. */
	@XmlElement(required = true, nillable = true)
	protected String craNumber;

	/** The password. */
	@XmlElement(required = true, nillable = true)
	protected String password;

	/** The request code. */
	@XmlElement(required = true, nillable = true)
	protected String requestCode;

	/** The user code. */
	@XmlElement(required = true, nillable = true)
	protected String userCode;

	/**
	 * Obtient la valeur de la propriété craNumber.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCraNumber() {

		return craNumber;
	}

	/**
	 * Définit la valeur de la propriété craNumber.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCraNumber(String value) {

		this.craNumber = value;
	}

	/**
	 * Obtient la valeur de la propriété password.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getPassword() {

		return password;
	}

	/**
	 * Définit la valeur de la propriété password.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setPassword(String value) {

		this.password = value;
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
	 * Obtient la valeur de la propriété userCode.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getUserCode() {

		return userCode;
	}

	/**
	 * Définit la valeur de la propriété userCode.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setUserCode(String value) {

		this.userCode = value;
	}

}

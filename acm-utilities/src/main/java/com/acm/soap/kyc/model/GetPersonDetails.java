/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.soap.kyc.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Classe Java pour anonymous complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="User_Code" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="password" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="referenceNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="nationalIDNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {"userCode", "password", "referenceNumber", "nationalIDNumber"})
@XmlRootElement(name = "get_PersonDetails")
public class GetPersonDetails {

	/** The user code. */
	@XmlElement(name = "User_Code", required = true)
	protected String userCode;

	/** The password. */
	@XmlElement(required = true)
	protected String password;

	/** The reference number. */
	@XmlElement(required = true)
	protected String referenceNumber;

	/** The national ID number. */
	@XmlElement(required = true)
	protected String nationalIDNumber;

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
	 * Obtient la valeur de la propriété referenceNumber.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getReferenceNumber() {

		return referenceNumber;
	}

	/**
	 * Définit la valeur de la propriété referenceNumber.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setReferenceNumber(String value) {

		this.referenceNumber = value;
	}

	/**
	 * Obtient la valeur de la propriété nationalIDNumber.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getNationalIDNumber() {

		return nationalIDNumber;
	}

	/**
	 * Définit la valeur de la propriété nationalIDNumber.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setNationalIDNumber(String value) {

		this.nationalIDNumber = value;
	}

}

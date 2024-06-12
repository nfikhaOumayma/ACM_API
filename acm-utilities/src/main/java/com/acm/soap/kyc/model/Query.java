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
 * Classe Java pour Query complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="Query"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="classCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="companyUniqueIdentifier" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="nationalId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="officeCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="registrationNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Query", propOrder = {"classCode", "companyUniqueIdentifier", "nationalId",
		"officeCode", "registrationNumber"})
public class Query {

	/** The class code. */
	@XmlElement(required = true, nillable = true)
	protected String classCode;

	/** The company unique identifier. */
	@XmlElement(required = true, nillable = true)
	protected String companyUniqueIdentifier;

	/** The national id. */
	@XmlElement(required = true, nillable = true)
	protected String nationalId;

	/** The office code. */
	@XmlElement(required = true, nillable = true)
	protected String officeCode;

	/** The registration number. */
	@XmlElement(required = true, nillable = true)
	protected String registrationNumber;

	/**
	 * Obtient la valeur de la propriété classCode.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getClassCode() {

		return classCode;
	}

	/**
	 * Définit la valeur de la propriété classCode.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setClassCode(String value) {

		this.classCode = value;
	}

	/**
	 * Obtient la valeur de la propriété companyUniqueIdentifier.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCompanyUniqueIdentifier() {

		return companyUniqueIdentifier;
	}

	/**
	 * Définit la valeur de la propriété companyUniqueIdentifier.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCompanyUniqueIdentifier(String value) {

		this.companyUniqueIdentifier = value;
	}

	/**
	 * Obtient la valeur de la propriété nationalId.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getNationalId() {

		return nationalId;
	}

	/**
	 * Définit la valeur de la propriété nationalId.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setNationalId(String value) {

		this.nationalId = value;
	}

	/**
	 * Obtient la valeur de la propriété officeCode.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getOfficeCode() {

		return officeCode;
	}

	/**
	 * Définit la valeur de la propriété officeCode.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setOfficeCode(String value) {

		this.officeCode = value;
	}

	/**
	 * Obtient la valeur de la propriété registrationNumber.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getRegistrationNumber() {

		return registrationNumber;
	}

	/**
	 * Définit la valeur de la propriété registrationNumber.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setRegistrationNumber(String value) {

		this.registrationNumber = value;
	}

}

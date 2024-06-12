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
 * Classe Java pour PersonDetails complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="PersonDetails"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="address" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="birthDate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="cardExpirationDate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="errorCode" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="errorDescription" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="familyName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="fcn" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="gender" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="governorate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="motherFamilyName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="motherFirstName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="nationalIDNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="personName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="policeStation" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="referenceNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="requestNumber" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="requestTimeStamp" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="userCode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PersonDetails",
		propOrder = {"address", "birthDate", "cardExpirationDate", "errorCode", "errorDescription",
				"familyName", "fcn", "gender", "governorate", "motherFamilyName", "motherFirstName",
				"nationalIDNumber", "personName", "policeStation", "referenceNumber",
				"requestNumber", "requestTimeStamp", "userCode"})
public class PersonDetails {

	/** The address. */
	@XmlElement(required = true, nillable = true)
	protected String address;

	/** The birth date. */
	@XmlElement(required = true, nillable = true)
	protected String birthDate;

	/** The card expiration date. */
	@XmlElement(required = true, nillable = true)
	protected String cardExpirationDate;

	/** The error code. */
	protected int errorCode;

	/** The error description. */
	@XmlElement(required = true, nillable = true)
	protected String errorDescription;

	/** The family name. */
	@XmlElement(required = true, nillable = true)
	protected String familyName;

	/** The fcn. */
	@XmlElement(required = true, nillable = true)
	protected String fcn;

	/** The gender. */
	@XmlElement(required = true, nillable = true)
	protected String gender;

	/** The governorate. */
	@XmlElement(required = true, nillable = true)
	protected String governorate;

	/** The mother family name. */
	@XmlElement(required = true, nillable = true)
	protected String motherFamilyName;

	/** The mother first name. */
	@XmlElement(required = true, nillable = true)
	protected String motherFirstName;

	/** The national ID number. */
	@XmlElement(required = true, nillable = true)
	protected String nationalIDNumber;

	/** The person name. */
	@XmlElement(required = true, nillable = true)
	protected String personName;

	/** The police station. */
	@XmlElement(required = true, nillable = true)
	protected String policeStation;

	/** The reference number. */
	@XmlElement(required = true, nillable = true)
	protected String referenceNumber;

	/** The request number. */
	@XmlElement(required = true, nillable = true)
	protected String requestNumber;

	/** The request time stamp. */
	@XmlElement(required = true, nillable = true)
	protected String requestTimeStamp;

	/** The user code. */
	@XmlElement(required = true, nillable = true)
	protected String userCode;

	/**
	 * Obtient la valeur de la propriété address.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getAddress() {

		return address;
	}

	/**
	 * Définit la valeur de la propriété address.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setAddress(String value) {

		this.address = value;
	}

	/**
	 * Obtient la valeur de la propriété birthDate.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getBirthDate() {

		return birthDate;
	}

	/**
	 * Définit la valeur de la propriété birthDate.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setBirthDate(String value) {

		this.birthDate = value;
	}

	/**
	 * Obtient la valeur de la propriété cardExpirationDate.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCardExpirationDate() {

		return cardExpirationDate;
	}

	/**
	 * Définit la valeur de la propriété cardExpirationDate.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCardExpirationDate(String value) {

		this.cardExpirationDate = value;
	}

	/**
	 * Obtient la valeur de la propriété errorCode.
	 *
	 * @return the error code
	 */
	public int getErrorCode() {

		return errorCode;
	}

	/**
	 * Définit la valeur de la propriété errorCode.
	 *
	 * @param value the new error code
	 */
	public void setErrorCode(int value) {

		this.errorCode = value;
	}

	/**
	 * Obtient la valeur de la propriété errorDescription.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getErrorDescription() {

		return errorDescription;
	}

	/**
	 * Définit la valeur de la propriété errorDescription.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setErrorDescription(String value) {

		this.errorDescription = value;
	}

	/**
	 * Obtient la valeur de la propriété familyName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getFamilyName() {

		return familyName;
	}

	/**
	 * Définit la valeur de la propriété familyName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setFamilyName(String value) {

		this.familyName = value;
	}

	/**
	 * Obtient la valeur de la propriété fcn.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getFcn() {

		return fcn;
	}

	/**
	 * Définit la valeur de la propriété fcn.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setFcn(String value) {

		this.fcn = value;
	}

	/**
	 * Obtient la valeur de la propriété gender.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getGender() {

		return gender;
	}

	/**
	 * Définit la valeur de la propriété gender.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setGender(String value) {

		this.gender = value;
	}

	/**
	 * Obtient la valeur de la propriété governorate.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getGovernorate() {

		return governorate;
	}

	/**
	 * Définit la valeur de la propriété governorate.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setGovernorate(String value) {

		this.governorate = value;
	}

	/**
	 * Obtient la valeur de la propriété motherFamilyName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getMotherFamilyName() {

		return motherFamilyName;
	}

	/**
	 * Définit la valeur de la propriété motherFamilyName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setMotherFamilyName(String value) {

		this.motherFamilyName = value;
	}

	/**
	 * Obtient la valeur de la propriété motherFirstName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getMotherFirstName() {

		return motherFirstName;
	}

	/**
	 * Définit la valeur de la propriété motherFirstName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setMotherFirstName(String value) {

		this.motherFirstName = value;
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

	/**
	 * Obtient la valeur de la propriété personName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getPersonName() {

		return personName;
	}

	/**
	 * Définit la valeur de la propriété personName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setPersonName(String value) {

		this.personName = value;
	}

	/**
	 * Obtient la valeur de la propriété policeStation.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getPoliceStation() {

		return policeStation;
	}

	/**
	 * Définit la valeur de la propriété policeStation.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setPoliceStation(String value) {

		this.policeStation = value;
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
	 * Obtient la valeur de la propriété requestNumber.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getRequestNumber() {

		return requestNumber;
	}

	/**
	 * Définit la valeur de la propriété requestNumber.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setRequestNumber(String value) {

		this.requestNumber = value;
	}

	/**
	 * Obtient la valeur de la propriété requestTimeStamp.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getRequestTimeStamp() {

		return requestTimeStamp;
	}

	/**
	 * Définit la valeur de la propriété requestTimeStamp.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setRequestTimeStamp(String value) {

		this.requestTimeStamp = value;
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

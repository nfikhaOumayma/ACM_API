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
 *         &lt;element name="get_PersonDetailsReturn" type="{http://main}PersonDetails"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt; 
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {"getPersonDetailsReturn"})
@XmlRootElement(name = "get_PersonDetailsResponse")
public class GetPersonDetailsResponse {

	/** The get person details return. */
	@XmlElement(name = "get_PersonDetailsReturn", required = true)
	protected PersonDetails getPersonDetailsReturn;

	/**
	 * Obtient la valeur de la propriété getPersonDetailsReturn.
	 * 
	 * @return possible object is {@link PersonDetails }
	 */
	public PersonDetails getGetPersonDetailsReturn() {

		return getPersonDetailsReturn;
	}

	/**
	 * Définit la valeur de la propriété getPersonDetailsReturn.
	 * 
	 * @param value allowed object is {@link PersonDetails }
	 */
	public void setGetPersonDetailsReturn(PersonDetails value) {

		this.getPersonDetailsReturn = value;
	}

}

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
 *         &lt;element name="CompanyDocumentRequest" type="{http://main}CompanyDocumentRequest"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {"companyDocumentRequest"})
@XmlRootElement(name = "getCompanyDocument")
public class GetCompanyDocument {

	/** The company document request. */
	@XmlElement(name = "CompanyDocumentRequest", required = true)
	protected CompanyDocumentRequest companyDocumentRequest;

	/**
	 * Obtient la valeur de la propriété companyDocumentRequest.
	 * 
	 * @return possible object is {@link CompanyDocumentRequest }
	 */
	public CompanyDocumentRequest getCompanyDocumentRequest() {

		return companyDocumentRequest;
	}

	/**
	 * Définit la valeur de la propriété companyDocumentRequest.
	 * 
	 * @param value allowed object is {@link CompanyDocumentRequest }
	 */
	public void setCompanyDocumentRequest(CompanyDocumentRequest value) {

		this.companyDocumentRequest = value;
	}

}

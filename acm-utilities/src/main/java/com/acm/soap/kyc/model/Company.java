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
 * Classe Java pour Company complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="Company"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="companyCEO" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="companyName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="companyStartActivityDate" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="cra" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Company",
		propOrder = {"companyCEO", "companyName", "companyStartActivityDate", "cra"})
public class Company {

	/** The company CEO. */
	@XmlElement(required = true, nillable = true)
	protected String companyCEO;

	/** The company name. */
	@XmlElement(required = true, nillable = true)
	protected String companyName;

	/** The company start activity date. */
	@XmlElement(required = true, nillable = true)
	protected String companyStartActivityDate;

	/** The cra. */
	@XmlElement(required = true, nillable = true)
	protected String cra;

	/**
	 * Obtient la valeur de la propriété companyCEO.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCompanyCEO() {

		return companyCEO;
	}

	/**
	 * Définit la valeur de la propriété companyCEO.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCompanyCEO(String value) {

		this.companyCEO = value;
	}

	/**
	 * Obtient la valeur de la propriété companyName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCompanyName() {

		return companyName;
	}

	/**
	 * Définit la valeur de la propriété companyName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCompanyName(String value) {

		this.companyName = value;
	}

	/**
	 * Obtient la valeur de la propriété companyStartActivityDate.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCompanyStartActivityDate() {

		return companyStartActivityDate;
	}

	/**
	 * Définit la valeur de la propriété companyStartActivityDate.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCompanyStartActivityDate(String value) {

		this.companyStartActivityDate = value;
	}

	/**
	 * Obtient la valeur de la propriété cra.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getCra() {

		return cra;
	}

	/**
	 * Définit la valeur de la propriété cra.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setCra(String value) {

		this.cra = value;
	}

}

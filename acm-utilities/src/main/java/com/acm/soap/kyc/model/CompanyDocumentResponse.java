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
 * Classe Java pour CompanyDocumentResponse complex type.
 * <p>
 * Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="CompanyDocumentResponse"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="enchodingType" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="file" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="fileExtention" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="fileName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
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
@XmlType(name = "CompanyDocumentResponse", propOrder = {"enchodingType", "file", "fileExtention",
		"fileName", "requestCode", "responseCode", "responseDesc"})
public class CompanyDocumentResponse {

	/** The enchoding type. */
	@XmlElement(required = true, nillable = true)
	protected String enchodingType;

	/** The file. */
	@XmlElement(required = true, nillable = true)
	protected String file;

	/** The file extention. */
	@XmlElement(required = true, nillable = true)
	protected String fileExtention;

	/** The file name. */
	@XmlElement(required = true, nillable = true)
	protected String fileName;

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
	 * Obtient la valeur de la propriété enchodingType.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getEnchodingType() {

		return enchodingType;
	}

	/**
	 * Définit la valeur de la propriété enchodingType.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setEnchodingType(String value) {

		this.enchodingType = value;
	}

	/**
	 * Obtient la valeur de la propriété file.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getFile() {

		return file;
	}

	/**
	 * Définit la valeur de la propriété file.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setFile(String value) {

		this.file = value;
	}

	/**
	 * Obtient la valeur de la propriété fileExtention.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getFileExtention() {

		return fileExtention;
	}

	/**
	 * Définit la valeur de la propriété fileExtention.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setFileExtention(String value) {

		this.fileExtention = value;
	}

	/**
	 * Obtient la valeur de la propriété fileName.
	 * 
	 * @return possible object is {@link String }
	 */
	public String getFileName() {

		return fileName;
	}

	/**
	 * Définit la valeur de la propriété fileName.
	 * 
	 * @param value allowed object is {@link String }
	 */
	public void setFileName(String value) {

		this.fileName = value;
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

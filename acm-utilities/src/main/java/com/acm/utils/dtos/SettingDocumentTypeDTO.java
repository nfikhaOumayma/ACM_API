/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * {@link SettingDocumentTypeDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SettingDocumentTypeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1541729254151486597L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The libelle. */
	private String libelle;

	/** The categorie -- LOAN : 0 || -- CLIENT : 1 || --COLLECTION :4 || --LEGAL : 5. */
	private Integer categorie;

	/** The categorie libelle. */
	private String categorieLibelle;

	/** The uniqueness. */
	private Boolean uniqueness;

	/** The mandatory. */
	private Boolean mandatory;

	/** The description. */
	private String description;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/** The enabled. */
	private Boolean enabled;

	/** The report name. */
	private String reportName;

	/** The name. */
	private String name;

	/** The id document GED. */
	private String idDocumentGED;

	/** The collections. */
	private List<CollectionStepDTO> collections = new ArrayList<>();

	/**
	 * Instantiates a new setting document type DTO.
	 */
	public SettingDocumentTypeDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting document type DTO.
	 *
	 * @param categorie the categorie
	 */
	public SettingDocumentTypeDTO(Integer categorie) {

		this.categorie = categorie;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the libelle.
	 *
	 * @return the libelle
	 */
	public String getLibelle() {

		return libelle;
	}

	/**
	 * Sets the libelle.
	 *
	 * @param libelle the libelle to set
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
	}

	/**
	 * Gets the categorie -- LOAN : 0 || -- CLIENT : 1.
	 *
	 * @return the categorie
	 */
	public Integer getCategorie() {

		return categorie;
	}

	/**
	 * Sets the categorie.
	 *
	 * @param categorie the categorie to set
	 */
	public void setCategorie(Integer categorie) {

		this.categorie = categorie;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the date debut.
	 *
	 * @return the dateDebut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the date debut.
	 *
	 * @param dateDebut the dateDebut to set
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the date fin.
	 *
	 * @return the dateFin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the date fin.
	 *
	 * @param dateFin the dateFin to set
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
	}

	/**
	 * Gets the categorie libelle.
	 *
	 * @return the categorieLibelle
	 */
	public String getCategorieLibelle() {

		// -- LOAN : 0 -- CLIENT : 1 -- ASSIGN_DOCUMENT : 2
		if (this.categorie != null) {
			switch (this.categorie) {
				case 0:
					this.categorieLibelle = "LOAN";
					break;
				case 1:
					this.categorieLibelle = "CLIENT";
					break;
				case 2:
					this.categorieLibelle = "ASSIGN_DOCUMENT";
					break;
				default:
					break;
			}
		}
		return categorieLibelle;
	}

	/**
	 * Sets the categorie libelle.
	 *
	 * @param categorieLibelle the categorieLibelle to set
	 */
	public void setCategorieLibelle(String categorieLibelle) {

		this.categorieLibelle = categorieLibelle;
	}

	/**
	 * Gets the uniqueness.
	 *
	 * @return the uniqueness
	 */
	public Boolean getUniqueness() {

		return uniqueness;
	}

	/**
	 * Sets the uniqueness.
	 *
	 * @param uniqueness the uniqueness to set
	 */
	public void setUniqueness(Boolean uniqueness) {

		this.uniqueness = uniqueness;
	}

	/**
	 * Gets the mandatory.
	 *
	 * @return the mandatory
	 */
	public Boolean getMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
	}

	/**
	 * Gets the enable.
	 * 
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enable.
	 * 
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the collections.
	 *
	 * @return the collections
	 */
	public List<CollectionStepDTO> getCollections() {

		return collections;
	}

	/**
	 * Sets the collections.
	 *
	 * @param collections the new collections
	 */
	public void setCollections(List<CollectionStepDTO> collections) {

		this.collections = collections;
	}

	/**
	 * Gets the report name.
	 *
	 * @return the report name
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the new report name
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the new name
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the id document GED.
	 *
	 * @return the id document GED
	 */
	public String getIdDocumentGED() {

		return idDocumentGED;
	}

	/**
	 * Sets the id document GED.
	 *
	 * @param idDocumentGED the new id document GED
	 */
	public void setIdDocumentGED(String idDocumentGED) {

		this.idDocumentGED = idDocumentGED;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "SettingDocumentTypeDTO [" + (id != null ? "id=" + id + ", " : "")
				+ (code != null ? "code=" + code + ", " : "")
				+ (libelle != null ? "libelle=" + libelle + ", " : "")
				+ (categorie != null ? "categorie=" + categorie + ", " : "")
				+ (categorieLibelle != null ? "categorieLibelle=" + categorieLibelle + ", " : "")
				+ (uniqueness != null ? "uniqueness=" + uniqueness + ", " : "")
				+ (mandatory != null ? "mandatory=" + mandatory + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (enabled != null ? "enabled=" + enabled : "") + "]";
	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link SettingDocumentType} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Entity
@Table(name = "ACM_SETTING_DOC_TYPE")
public class SettingDocumentType extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3764176307092442364L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_DOC_TYPE", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/**
	 * The categorie. -- LOAN : 0 || -- CLIENT : 1 || -- ASSIGN_DOCUMENT : 2.
	 */
	@Column(name = "CATEGORIE", nullable = false)
	private Integer categorie;

	/** The uniqueness. */
	@Column(name = "UNIQUENESS")
	private Boolean uniqueness;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The date debut. */
	@Column(name = "DATE_DEBUT")
	private Date dateDebut;

	/** The date fin. */
	@Column(name = "DATE_FIN")
	private Date dateFin;

	/** The setting document products. */
	@OneToMany(mappedBy = "settingDocumentType")
	private Set<SettingDocumentProduct> settingDocumentProducts = new HashSet<>();

	/** The acm documents. */
	@OneToMany(mappedBy = "settingDocumentType")
	private Set<AcmDocuments> acmDocuments = new HashSet<>();

	/** The work flow steps. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "documentTypes")
	private Set<WorkFlowStep> workFlowSteps = new HashSet<>();

	/**
	 * Instantiates a new setting document type.
	 */
	public SettingDocumentType() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting document type.
	 *
	 * @param id the id
	 */
	public SettingDocumentType(Long id) {

		this.id = id;
	}

	/**
	 * Gets the acm documents.
	 *
	 * @return the acmDocuments
	 */
	public Set<AcmDocuments> getAcmDocuments() {

		return acmDocuments;
	}

	/**
	 * Sets the acm documents.
	 *
	 * @param acmDocuments the acmDocuments to set
	 */
	public void setAcmDocuments(Set<AcmDocuments> acmDocuments) {

		this.acmDocuments = acmDocuments;
	}

	/**
	 * Gets the setting document products.
	 *
	 * @return the settingDocumentProducts
	 */
	public Set<SettingDocumentProduct> getSettingDocumentProducts() {

		return settingDocumentProducts;
	}

	/**
	 * Sets the setting document products.
	 *
	 * @param settingDocumentProducts the settingDocumentProducts to set
	 */
	public void setSettingDocumentProducts(Set<SettingDocumentProduct> settingDocumentProducts) {

		this.settingDocumentProducts = settingDocumentProducts;
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
	 * Gets the work flow steps.
	 *
	 * @return the work flow steps
	 */
	public Set<WorkFlowStep> getWorkFlowSteps() {

		return workFlowSteps;
	}

	/**
	 * Sets the work flow steps.
	 *
	 * @param workFlowSteps the new work flow steps
	 */
	public void setWorkFlowSteps(Set<WorkFlowStep> workFlowSteps) {

		this.workFlowSteps = workFlowSteps;
	}

}

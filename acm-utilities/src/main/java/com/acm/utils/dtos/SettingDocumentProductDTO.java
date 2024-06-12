/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link SettingDocumentProductDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
public class SettingDocumentProductDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7063036038941213498L;

	/** The id. */
	private Long id;

	/** The setting document type DTO. */
	@Mapping("settingDocumentType")
	private SettingDocumentTypeDTO settingDocumentTypeDTO;

	/** The product id. */
	private Integer productId;

	/** The mandatory. */
	private Boolean mandatory;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * The all. True : get all setting product / False : get only when enable true.
	 */
	private Boolean all;

	/** The report name. */
	private String reportName;

	/**
	 * Instantiates a new setting document product DTO.
	 */
	public SettingDocumentProductDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting document product DTO.
	 *
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @param productId the product id
	 * @param mandatory the mandatory
	 */
	public SettingDocumentProductDTO(SettingDocumentTypeDTO settingDocumentTypeDTO,
			Integer productId, Boolean mandatory) {

		this.settingDocumentTypeDTO = settingDocumentTypeDTO;
		this.productId = productId;
		this.mandatory = mandatory;
	}

	/**
	 * Instantiates a new setting document product DTO.
	 *
	 * @param id the id
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @param productId the product id
	 */
	public SettingDocumentProductDTO(Long id, SettingDocumentTypeDTO settingDocumentTypeDTO,
			Integer productId) {

		this.id = id;
		this.settingDocumentTypeDTO = settingDocumentTypeDTO;
		this.productId = productId;
	}

	/**
	 * Instantiates a new setting document product DTO.
	 *
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @param productId the product id
	 * @param mandatory the mandatory
	 * @param dateDebut the date debut
	 * @param reportName the report name
	 */
	public SettingDocumentProductDTO(SettingDocumentTypeDTO settingDocumentTypeDTO,
			Integer productId, Boolean mandatory, Date dateDebut, String reportName) {

		this.settingDocumentTypeDTO = settingDocumentTypeDTO;
		this.productId = productId;
		this.mandatory = mandatory;
		this.dateDebut = dateDebut;
		this.reportName = reportName;
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
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Integer getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Integer productId) {

		this.productId = productId;
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
	 * Gets the setting document type DTO.
	 *
	 * @return the settingDocumentTypeDTO
	 */
	public SettingDocumentTypeDTO getSettingDocumentTypeDTO() {

		return settingDocumentTypeDTO;
	}

	/**
	 * Sets the setting document type DTO.
	 *
	 * @param settingDocumentTypeDTO the settingDocumentTypeDTO to set
	 */
	public void setSettingDocumentTypeDTO(SettingDocumentTypeDTO settingDocumentTypeDTO) {

		this.settingDocumentTypeDTO = settingDocumentTypeDTO;
	}

	/**
	 * Gets the enabled.
	 * 
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 * 
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the all.
	 * 
	 * @return the all
	 */
	public Boolean getAll() {

		return all;
	}

	/**
	 * Sets the all.
	 * 
	 * @param all the all to set
	 */
	public void setAll(Boolean all) {

		this.all = all;
	}

	/**
	 * Gets the report name.
	 *
	 * @return the reportName
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the reportName to set
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

}

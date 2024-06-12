/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

import org.dozer.Mapping;

/**
 * {@link SettingLevelProcessDTO} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
public class SettingLevelProcessDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7617583341449787362L;

	/** The id. */
	private Long id;

	/** The id product. */
	private Long idProduct;

	/** The amount. */
	private BigDecimal amount;

	/** The description. */
	private String description;

	/** The enabled. */
	private Boolean enabled;

	/** The setting level DTO. */
	@Mapping("settingLevel")
	private SettingLevelDTO settingLevelDTO;

	/**
	 * Instantiates a new setting process level.
	 */
	public SettingLevelProcessDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting level process DTO.
	 *
	 * @param idProduct the id product
	 * @param amount the amount
	 */
	public SettingLevelProcessDTO(Long idProduct, BigDecimal amount) {

		this.idProduct = idProduct;
		this.amount = amount;
	}

	/**
	 * Instantiates a new setting level process DTO.
	 *
	 * @param idProduct the id product
	 * @param settingLevelDTO the setting level DTO
	 * @param amount the amount
	 */
	public SettingLevelProcessDTO(Long idProduct, SettingLevelDTO settingLevelDTO,
			BigDecimal amount) {

		this.idProduct = idProduct;
		this.settingLevelDTO = settingLevelDTO;
		this.amount = amount;
	}

	/**
	 * Instantiates a new setting level process DTO.
	 *
	 * @param idProduct the id product
	 * @param amount the amount
	 * @param description the description
	 * @param settingLevelDTO the setting level DTO
	 */
	public SettingLevelProcessDTO(Long idProduct, BigDecimal amount, String description,
			SettingLevelDTO settingLevelDTO) {

		this.idProduct = idProduct;
		this.amount = amount;
		this.description = description;
		this.settingLevelDTO = settingLevelDTO;
	}

	/**
	 * Instantiates a new setting level process DTO.
	 *
	 * @param settingLevelDTO the setting level DTO
	 */
	public SettingLevelProcessDTO(SettingLevelDTO settingLevelDTO) {

		this.settingLevelDTO = settingLevelDTO;
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
	 * Gets the id product.
	 *
	 * @return the idProduct
	 */
	public Long getIdProduct() {

		return idProduct;
	}

	/**
	 * Sets the id product.
	 *
	 * @param idProduct the idProduct to set
	 */
	public void setIdProduct(Long idProduct) {

		this.idProduct = idProduct;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
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
	 * Gets the setting level DTO.
	 *
	 * @return the settingLevelDTO
	 */
	public SettingLevelDTO getSettingLevelDTO() {

		return settingLevelDTO;
	}

	/**
	 * Sets the setting level DTO.
	 *
	 * @param settingLevelDTO the settingLevelDTO to set
	 */
	public void setSettingLevelDTO(SettingLevelDTO settingLevelDTO) {

		this.settingLevelDTO = settingLevelDTO;
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

}

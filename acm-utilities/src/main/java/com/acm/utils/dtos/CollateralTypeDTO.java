/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * {@link CollateralTypeDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class CollateralTypeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2673478035094354677L;

	/** The id. */
	private Long id;

	/** The id extern. */
	private Long idExtern;
	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The collection pourcentage. */
	private Long collectionPourcentage;

	/** The collection cost. */
	private BigDecimal collectionCost;

	/** The override collection data. */
	private Boolean overrideCollectionData;

	/** The default type. */
	private Boolean defaultType;

	/** The link to account. */
	private Boolean linkToAccount;

	/** The enabled. */
	private Boolean enabled;

	/** The products. */
	private String productIds;

	/**
	 * Instantiates a new collateral type DTO.
	 *
	 * @param idExtern the id extern
	 * @param code the code
	 * @param description the description
	 * @param collectionPourcentage the collection pourcentage
	 * @param collectionCost the collection cost
	 * @param overrideCollectionData the override collection data
	 * @param productIds the product ids
	 * @param defaultType the default type
	 * @param enabled the enabled
	 */
	public CollateralTypeDTO(Long idExtern, String code, String description,
			Long collectionPourcentage, BigDecimal collectionCost, Boolean overrideCollectionData,
			String productIds, Boolean defaultType, Boolean enabled) {

		this.idExtern = idExtern;
		this.code = code;
		this.description = description;
		this.collectionPourcentage = collectionPourcentage;
		this.collectionCost = collectionCost;
		this.overrideCollectionData = overrideCollectionData;
		this.defaultType = defaultType;
		this.enabled = enabled;
		this.productIds = productIds;
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
	 * @param id the new id
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
	 * @param code the new code
	 */
	public void setCode(String code) {

		this.code = code;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the collection pourcentage.
	 *
	 * @return the collection pourcentage
	 */
	public Long getCollectionPourcentage() {

		return collectionPourcentage;
	}

	/**
	 * Sets the collection pourcentage.
	 *
	 * @param collectionPourcentage the new collection pourcentage
	 */
	public void setCollectionPourcentage(Long collectionPourcentage) {

		this.collectionPourcentage = collectionPourcentage;
	}

	/**
	 * Gets the collection cost.
	 *
	 * @return the collection cost
	 */
	public BigDecimal getCollectionCost() {

		return collectionCost;
	}

	/**
	 * Sets the collection cost.
	 *
	 * @param collectionCost the new collection cost
	 */
	public void setCollectionCost(BigDecimal collectionCost) {

		this.collectionCost = collectionCost;
	}

	/**
	 * Gets the override collection data.
	 *
	 * @return the override collection data
	 */
	public Boolean getOverrideCollectionData() {

		return overrideCollectionData;
	}

	/**
	 * Sets the override collection data.
	 *
	 * @param overrideCollectionData the new override collection data
	 */
	public void setOverrideCollectionData(Boolean overrideCollectionData) {

		this.overrideCollectionData = overrideCollectionData;
	}

	/**
	 * Gets the default type.
	 *
	 * @return the default type
	 */
	public Boolean getDefaultType() {

		return defaultType;
	}

	/**
	 * Sets the default type.
	 *
	 * @param defaultType the new default type
	 */
	public void setDefaultType(Boolean defaultType) {

		this.defaultType = defaultType;
	}

	/**
	 * Gets the link to account.
	 *
	 * @return the link to account
	 */
	public Boolean getLinkToAccount() {

		return linkToAccount;
	}

	/**
	 * Sets the link to account.
	 *
	 * @param linkToAccount the new link to account
	 */
	public void setLinkToAccount(Boolean linkToAccount) {

		this.linkToAccount = linkToAccount;
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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the product ids.
	 *
	 * @return the product ids
	 */
	public String getProductIds() {

		return productIds;
	}

	/**
	 * Sets the product ids.
	 *
	 * @param productIds the new product ids
	 */
	public void setProductIds(String productIds) {

		this.productIds = productIds;
	}

	/**
	 * Gets the id extern.
	 *
	 * @return the id extern
	 */
	public Long getIdExtern() {

		return idExtern;
	}

	/**
	 * Sets the id extern.
	 *
	 * @param idExtern the new id extern
	 */
	public void setIdExtern(Long idExtern) {

		this.idExtern = idExtern;
	}
}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link AssetLoanDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class AssetLoanDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7100041789796035414L;

	/** The id. */
	private Long id;

	/** The id Asset. */
	private AssetDTO asset;

	/** The id loan. */
	@Mapping("loan.idLoan")
	private Long idLoan;

	/** The prix unitaire. */
	private BigDecimal prixUnitaire;

	/** The remise article. */
	private BigDecimal remiseArticle;

	/** The quantite article. */
	private Integer quantiteArticle;

	/** The insert by. */
	private String insertBy;

	/** The date insertion. */
	private Date dateInsertion;

	/** The enabled. */
	private Boolean enabled = true;

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
	 * Gets the asset.
	 *
	 * @return the asset
	 */
	public AssetDTO getAsset() {

		return asset;
	}

	/**
	 * Sets the asset.
	 *
	 * @param asset the new asset
	 */
	public void setAsset(AssetDTO asset) {

		this.asset = asset;
	}

	/**
	 * Gets the id loan.
	 *
	 * @return the id loan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the new id loan
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the prix unitaire.
	 *
	 * @return the prix unitaire
	 */
	public BigDecimal getPrixUnitaire() {

		return prixUnitaire;
	}

	/**
	 * Sets the prix unitaire.
	 *
	 * @param prixUnitaire the new prix unitaire
	 */
	public void setPrixUnitaire(BigDecimal prixUnitaire) {

		this.prixUnitaire = prixUnitaire;
	}

	/**
	 * Gets the remise article.
	 *
	 * @return the remise article
	 */
	public BigDecimal getRemiseArticle() {

		return remiseArticle;
	}

	/**
	 * Sets the remise article.
	 *
	 * @param remiseArticle the new remise article
	 */
	public void setRemiseArticle(BigDecimal remiseArticle) {

		this.remiseArticle = remiseArticle;
	}

	/**
	 * Gets the quantite article.
	 *
	 * @return the quantite article
	 */
	public Integer getQuantiteArticle() {

		return quantiteArticle;
	}

	/**
	 * Sets the quantite article.
	 *
	 * @param quantiteArticle the new quantite article
	 */
	public void setQuantiteArticle(Integer quantiteArticle) {

		this.quantiteArticle = quantiteArticle;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
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

}

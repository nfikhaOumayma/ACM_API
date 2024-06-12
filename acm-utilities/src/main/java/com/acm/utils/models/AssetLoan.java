/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link AssetLoan} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_ASSET_ACM_LOAN")
public class AssetLoan extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1190063523604154323L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_ASSET_ACM_LOAN", unique = true, nullable = false)
	private Long id;

	/** The asset id. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_ASSET")
	private Asset asset;

	/** The loan id. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/** The prix unitaire. */
	@Column(name = "PRIX_UNITAIRE")
	private BigDecimal prixUnitaire;

	/** The remise article. */
	@Column(name = "REMISE_ARTICLE")
	private BigDecimal remiseArticle;

	/** The quantite article. */
	@Column(name = "QUANTITE_ARTICLE")
	private Integer quantiteArticle;

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
	public Asset getAsset() {

		return asset;
	}

	/**
	 * Sets the asset.
	 *
	 * @param asset the new asset
	 */
	public void setAsset(Asset asset) {

		this.asset = asset;
	}

	/**
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the new loan
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
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

}

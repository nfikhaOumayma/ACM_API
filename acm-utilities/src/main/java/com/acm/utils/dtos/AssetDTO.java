/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link AssetDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class AssetDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7100041789796035414L;

	/** The id. */
	private Long id;

	/** The id fournisseur. */
	private SupplierDTO supplier;

	/** The supplier name. */
	private String supplierName;

	/** The code article. */
	private String codeArticle;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The prix unitaire. */
	private BigDecimal prixUnitaire;

	/** The remise article. */
	private BigDecimal remiseArticle;

	/** The insert by. */
	private String insertBy;

	/** The date insertion. */
	private Date dateInsertion;

	/** The reserved stock. */
	private String reservedStock;

	/** The promotion. */
	private String promotion;

	/** The promotion start date. */
	private Date promotionStartDate;

	/** The promotion end date. */
	private Date promotionEndDate;

	/** The tva. */
	private Long tva;

	/** The type asset. */
	private String typeAsset;

	/** The category asset. */
	private String categoryAsset;

	/** The sub category asset. */
	private String subCategoryAsset;

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
	 * Gets the supplier.
	 *
	 * @return the supplier
	 */
	public SupplierDTO getSupplier() {

		return supplier;
	}

	/**
	 * Sets the supplier.
	 *
	 * @param supplier the new supplier
	 */
	public void setSupplier(SupplierDTO supplier) {

		this.supplier = supplier;
	}

	/**
	 * Gets the code article.
	 *
	 * @return the code article
	 */
	public String getCodeArticle() {

		return codeArticle;
	}

	/**
	 * Sets the code article.
	 *
	 * @param codeArticle the new code article
	 */
	public void setCodeArticle(String codeArticle) {

		this.codeArticle = codeArticle;
	}

	/**
	 * Gets the date debut.
	 *
	 * @return the date debut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the date debut.
	 *
	 * @param dateDebut the new date debut
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the date fin.
	 *
	 * @return the date fin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the date fin.
	 *
	 * @param dateFin the new date fin
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
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
	 * @param libelle the new libelle
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
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
	 * Gets the supplier name.
	 *
	 * @return the supplier name
	 */
	public String getSupplierName() {

		return supplierName;
	}

	/**
	 * Sets the supplier name.
	 *
	 * @param supplierName the new supplier name
	 */
	public void setSupplierName(String supplierName) {

		this.supplierName = supplierName;
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
	 * Gets the reserved stock.
	 *
	 * @return the reserved stock
	 */
	public String getReservedStock() {

		return reservedStock;
	}

	/**
	 * Sets the reserved stock.
	 *
	 * @param reservedStock the new reserved stock
	 */
	public void setReservedStock(String reservedStock) {

		this.reservedStock = reservedStock;
	}

	/**
	 * Gets the promotion.
	 *
	 * @return the promotion
	 */
	public String getPromotion() {

		return promotion;
	}

	/**
	 * Sets the promotion.
	 *
	 * @param promotion the new promotion
	 */
	public void setPromotion(String promotion) {

		this.promotion = promotion;
	}

	/**
	 * Gets the promotion start date.
	 *
	 * @return the promotion start date
	 */
	public Date getPromotionStartDate() {

		return promotionStartDate;
	}

	/**
	 * Sets the promotion start date.
	 *
	 * @param promotionStartDate the new promotion start date
	 */
	public void setPromotionStartDate(Date promotionStartDate) {

		this.promotionStartDate = promotionStartDate;
	}

	/**
	 * Gets the promotion end date.
	 *
	 * @return the promotion end date
	 */
	public Date getPromotionEndDate() {

		return promotionEndDate;
	}

	/**
	 * Sets the promotion end date.
	 *
	 * @param promotionEndDate the new promotion end date
	 */
	public void setPromotionEndDate(Date promotionEndDate) {

		this.promotionEndDate = promotionEndDate;
	}

	/**
	 * Gets the tva.
	 *
	 * @return the tva
	 */
	public Long getTva() {

		return tva;
	}

	/**
	 * Sets the tva.
	 *
	 * @param tva the new tva
	 */
	public void setTva(Long tva) {

		this.tva = tva;
	}

	/**
	 * Gets the type asset.
	 *
	 * @return the type asset
	 */
	public String getTypeAsset() {

		return typeAsset;
	}

	/**
	 * Sets the type asset.
	 *
	 * @param typeAsset the new type asset
	 */
	public void setTypeAsset(String typeAsset) {

		this.typeAsset = typeAsset;
	}

	/**
	 * Gets the category asset.
	 *
	 * @return the category asset
	 */
	public String getCategoryAsset() {

		return categoryAsset;
	}

	/**
	 * Sets the category asset.
	 *
	 * @param categoryAsset the new category asset
	 */
	public void setCategoryAsset(String categoryAsset) {

		this.categoryAsset = categoryAsset;
	}

	/**
	 * Gets the sub category asset.
	 *
	 * @return the sub category asset
	 */
	public String getSubCategoryAsset() {

		return subCategoryAsset;
	}

	/**
	 * Sets the sub category asset.
	 *
	 * @param subCategoryAsset the new sub category asset
	 */
	public void setSubCategoryAsset(String subCategoryAsset) {

		this.subCategoryAsset = subCategoryAsset;
	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

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
 * {@link ItemRiskSetting} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_ITEM_RISQUE_SETTING")
public class ItemRiskSetting implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ITEM_INSTANCE_RISQUE_SETTING", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_ACM_ITEM")
	private Item item;

	/** The setting type risk. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_TYPE_RISQUE")
	private SettingTypeRisk settingTypeRisk;

	/** The id selected echelle. */
	@Column(name = "ID_SELECTED_ECHELLE")
	private Long idSelectedEchelle;

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
	 * Gets the item.
	 *
	 * @return the item
	 */
	public Item getItem() {

		return item;
	}

	/**
	 * Sets the item.
	 *
	 * @param item the new item
	 */
	public void setItem(Item item) {

		this.item = item;
	}

	/**
	 * Gets the setting type risk.
	 *
	 * @return the setting type risk
	 */
	public SettingTypeRisk getSettingTypeRisk() {

		return settingTypeRisk;
	}

	/**
	 * Sets the setting type risk.
	 *
	 * @param settingTypeRisk the new setting type risk
	 */
	public void setSettingTypeRisk(SettingTypeRisk settingTypeRisk) {

		this.settingTypeRisk = settingTypeRisk;
	}

	/**
	 * Gets the id selected echelle.
	 *
	 * @return the id selected echelle
	 */
	public Long getIdSelectedEchelle() {

		return idSelectedEchelle;
	}

	/**
	 * Sets the id selected echelle.
	 *
	 * @param idSelectedEchelle the new id selected echelle
	 */
	public void setIdSelectedEchelle(Long idSelectedEchelle) {

		this.idSelectedEchelle = idSelectedEchelle;
	}

}

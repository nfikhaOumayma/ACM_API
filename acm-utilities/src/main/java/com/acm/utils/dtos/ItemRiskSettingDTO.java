/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.models.ItemRiskSetting;

/**
 * {@link ItemRiskSetting} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */

public class ItemRiskSettingDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */

	private Long id;

	/** The Item instance. */
	private ItemDTO Item;

	/** The setting type risk. */
	private SettingTypeRiskDTO settingTypeRisk;

	/** The editable. */
	private Boolean editable;

	/** The id selected echelle. */
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
	public ItemDTO getItem() {

		return Item;
	}

	/**
	 * Sets the item.
	 *
	 * @param item the new item
	 */
	public void setItem(ItemDTO item) {

		Item = item;
	}

	/**
	 * Gets the setting type risk.
	 *
	 * @return the setting type risk
	 */
	public SettingTypeRiskDTO getSettingTypeRisk() {

		return settingTypeRisk;
	}

	/**
	 * Sets the setting type risk.
	 *
	 * @param settingTypeRisk the new setting type risk
	 */
	public void setSettingTypeRisk(SettingTypeRiskDTO settingTypeRisk) {

		this.settingTypeRisk = settingTypeRisk;
	}

	/**
	 * Gets the editable.
	 *
	 * @return the editable
	 */
	public Boolean getEditable() {

		return editable;
	}

	/**
	 * Sets the editable.
	 *
	 * @param editable the new editable
	 */
	public void setEditable(Boolean editable) {

		this.editable = editable;
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

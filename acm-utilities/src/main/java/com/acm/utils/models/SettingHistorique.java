/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link SettingHistorique} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Entity
@Table(name = "ACM_SETTING_HISTORIQUE")
public class SettingHistorique implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4840373549172570228L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_HISTORIQUE", unique = true, nullable = false)
	private Long id;

	/** The table name. */
	@Column(name = "TABLE_NAME", nullable = false)
	private String tableName;

	/** The action. */
	@Column(name = "ACTION", nullable = false)
	private String action;

	/** The id object. */
	@Column(name = "ID_OBJECT", nullable = false)
	private Long idObject;

	/** The updated data. */
	@Column(name = "UPDATED_DATA")
	private String updatedData;

	/** The new data. */
	@Column(name = "NEW_DATA")
	private String newData;

	/** The date update. */
	@Column(name = "DATE_UPDATE")
	private Date dateUpdate;

	/** The updated by. */
	@Column(name = "UPDATED_BY", length = 256)
	private String updatedBy;

	/**
	 * Instantiates a new setting historique.
	 */
	public SettingHistorique() {

		/*
		 * EMPTY
		 */
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
	 * Gets the table name.
	 *
	 * @return the tableName
	 */
	public String getTableName() {

		return tableName;
	}

	/**
	 * Sets the table name.
	 *
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {

		this.tableName = tableName;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the action to set
	 */
	public void setAction(String action) {

		this.action = action;
	}

	/**
	 * Gets the id object.
	 *
	 * @return the idObject
	 */
	public Long getIdObject() {

		return idObject;
	}

	/**
	 * Sets the id object.
	 *
	 * @param idObject the idObject to set
	 */
	public void setIdObject(Long idObject) {

		this.idObject = idObject;
	}

	/**
	 * Gets the updated data.
	 *
	 * @return the updatedData
	 */
	public String getUpdatedData() {

		return updatedData;
	}

	/**
	 * Sets the updated data.
	 *
	 * @param updatedData the updatedData to set
	 */
	public void setUpdatedData(String updatedData) {

		this.updatedData = updatedData;
	}

	/**
	 * Gets the new data.
	 *
	 * @return the newData
	 */
	public String getNewData() {

		return newData;
	}

	/**
	 * Sets the new data.
	 *
	 * @param newData the newData to set
	 */
	public void setNewData(String newData) {

		this.newData = newData;
	}

	/**
	 * Gets the date update.
	 *
	 * @return the dateUpdate
	 */
	public Date getDateUpdate() {

		return dateUpdate;
	}

	/**
	 * Sets the date update.
	 *
	 * @param dateUpdate the dateUpdate to set
	 */
	public void setDateUpdate(Date dateUpdate) {

		this.dateUpdate = dateUpdate;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updatedBy
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the updatedBy to set
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
	}

}

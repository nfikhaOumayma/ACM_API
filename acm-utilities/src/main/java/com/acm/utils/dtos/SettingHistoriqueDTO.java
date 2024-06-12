/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link SettingHistoriqueDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
public class SettingHistoriqueDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1850009598132108812L;

	/** The id. */
	private Long id;

	/** The table name. */
	private String tableName;

	/** The action. */
	private String action;

	/** The id object. */
	private Long idObject;

	/** The updated data. */
	private String updatedData;

	/** The new data. */
	private String newData;

	/** The date update. */
	private Date dateUpdate;

	/** The updated by. */
	private String updatedBy;

	/**
	 * Instantiates a new setting historique.
	 */
	public SettingHistoriqueDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting historique DTO (Used in ProcessHistorySettingAspectImpl for NEW
	 * ENTRY object).
	 *
	 * @param tableName the table name
	 * @param action the action
	 * @param idObject the id object
	 * @param newData the new data
	 * @param updatedBy the updated by
	 */
	public SettingHistoriqueDTO(String tableName, String action, Long idObject, String newData,
			String updatedBy) {

		this.tableName = tableName;
		this.action = action;
		this.idObject = idObject;
		this.newData = newData;
		this.updatedBy = updatedBy;
	}

	/**
	 * Instantiates a new setting historique DTO in UPDATE MODE.
	 *
	 * @param tableName the table name
	 * @param action the action
	 * @param idObject the id object
	 * @param updatedData the updated data
	 */
	public SettingHistoriqueDTO(String tableName, String action, Long idObject,
			String updatedData) {

		this.tableName = tableName;
		this.action = action;
		this.idObject = idObject;
		this.updatedData = updatedData;
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

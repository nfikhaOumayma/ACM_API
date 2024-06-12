/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The {@link SettingListValuesDTO} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class SettingListValuesDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -90871237074185750L;

	/** The id. */
	private Long id;

	/** The table abacus name. */
	private String tableAbacusName;

	/** The id extern. */
	private String idExtern;

	/** The value json. */
	private String valueJson;

	/** The parent id. */
	private Long parentId;

	/** The list name. */
	private String listName;

	/**
	 * Instantiates a new setting list values DTO.
	 */
	public SettingListValuesDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new setting list values DTO.
	 *
	 * @param tableAbacusName the table abacus name
	 */
	public SettingListValuesDTO(String tableAbacusName) {

		this.tableAbacusName = tableAbacusName;
	}

	/**
	 * Instantiates a new setting list values DTO.
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idExtern the id extern
	 * @param valueJson the value json
	 * @param parentId the parent id
	 */
	public SettingListValuesDTO(String tableAbacusName, String idExtern, String valueJson,
			Long parentId) {

		this.tableAbacusName = tableAbacusName;
		this.idExtern = idExtern;
		this.valueJson = valueJson;
		this.parentId = parentId;
	}

	/**
	 * Instantiates a new setting list values DTO.
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idExtern the id extern
	 * @param valueJson the value json
	 * @param parentId the parent id
	 * @param listName the list name
	 */
	public SettingListValuesDTO(String tableAbacusName, String idExtern, String valueJson,
			Long parentId, String listName) {

		this.tableAbacusName = tableAbacusName;
		this.idExtern = idExtern;
		this.valueJson = valueJson;
		this.parentId = parentId;
		this.listName = listName;
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
	 * Gets the parent id.
	 *
	 * @return the parentId
	 */
	public Long getParentId() {

		return parentId;
	}

	/**
	 * Sets the parent id.
	 *
	 * @param parentId the parentId to set
	 */
	public void setParentId(Long parentId) {

		this.parentId = parentId;
	}

	/**
	 * Gets the table abacus name.
	 *
	 * @return the tableAbacusName
	 */
	public String getTableAbacusName() {

		return tableAbacusName;
	}

	/**
	 * Sets the table abacus name.
	 *
	 * @param tableAbacusName the tableAbacusName to set
	 */
	public void setTableAbacusName(String tableAbacusName) {

		this.tableAbacusName = tableAbacusName;
	}

	/**
	 * Gets the id extern.
	 *
	 * @return the idExtern
	 */
	public String getIdExtern() {

		return idExtern;
	}

	/**
	 * Sets the id extern.
	 *
	 * @param idExtern the idExtern to set
	 */
	public void setIdExtern(String idExtern) {

		this.idExtern = idExtern;
	}

	/**
	 * Gets the value json.
	 *
	 * @return the valueJson
	 */
	public String getValueJson() {

		return valueJson;
	}

	/**
	 * Sets the value json.
	 *
	 * @param valueJson the valueJson to set
	 */
	public void setValueJson(String valueJson) {

		this.valueJson = valueJson;
	}

	/**
	 * Gets the list name.
	 *
	 * @return the list name
	 */
	public String getListName() {

		return listName;
	}

	/**
	 * Sets the list name.
	 *
	 * @param listName the new list name
	 */
	public void setListName(String listName) {

		this.listName = listName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "SettingListValues [id=" + id + ", tableAbacusName=" + tableAbacusName
				+ ", idExtern=" + idExtern + ", valueJson=" + valueJson + ", parentId=" + parentId
				+ "]";
	}

}

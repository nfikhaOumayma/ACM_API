/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.ParameterMode;
import javax.persistence.StoredProcedureParameter;
import javax.persistence.Table;

/**
 * The persistent class for the ACM_SETTING_LIST_VALUES database table. {@link SettingListValues}
 * class.
 * 
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_SETTING_LIST_VALUES")
@NamedStoredProcedureQuery(name = "ACM_PROCEDURE_UPDATE_BRANCHES",
		procedureName = "ACM_PROCEDURE_UPDATE_BRANCHES",
		parameters = {@StoredProcedureParameter(mode = ParameterMode.OUT, name = "count_out",
				type = Integer.class)})
public class SettingListValues extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1058507347346149486L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_LIST_VALUES", unique = true, nullable = false)
	private Long id;

	/** The table abacus name. */
	@Column(name = "TABLE_ABACUS_NAME", length = 512)
	private String tableAbacusName;

	/** The list name. */
	@Column(name = "LIST_NAME")
	private String listName;

	/** The id extern. */
	@Column(name = "ID_EXTERN", length = 256, nullable = false)
	private String idExtern;

	/** The value json. */
	@Column(name = "VALUE_JSON")
	private String valueJson;

	/** The parent id. */
	@Column(name = "PARENT_ID")
	private Long parentId;

	/** The Loans. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "lstFeesListValue")
	private Set<WorkFlowStep> workFlow = new HashSet<>();

	/**
	 * Instantiates a new setting list values.
	 */
	public SettingListValues() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new setting list values.
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idExtern the id extern
	 * @param valueJson the value json
	 * @param parentId the parent id
	 */
	public SettingListValues(String tableAbacusName, String idExtern, String valueJson,
			Long parentId) {

		this.tableAbacusName = tableAbacusName;
		this.idExtern = idExtern;
		this.valueJson = valueJson;
		this.parentId = parentId;
	}

	/**
	 * Instantiates a new setting list values.
	 *
	 * @param tableAbacusName the table abacus name
	 * @param listName the list name
	 * @param idExtern the id extern
	 * @param valueJson the value json
	 * @param parentId the parent id
	 */
	public SettingListValues(String tableAbacusName, String listName, String idExtern,
			String valueJson, Long parentId) {

		this.tableAbacusName = tableAbacusName;
		this.listName = listName;
		this.idExtern = idExtern;
		this.valueJson = valueJson;
		this.parentId = parentId;
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
	 * Gets the work flow.
	 *
	 * @return the workFlow
	 */
	public Set<WorkFlowStep> getWorkFlow() {

		return workFlow;
	}

	/**
	 * Sets the work flow.
	 *
	 * @param workFlow the workFlow to set
	 */
	public void setWorkFlow(Set<WorkFlowStep> workFlow) {

		this.workFlow = workFlow;
	}

	/**
	 * To string.
	 *
	 * @return the string
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

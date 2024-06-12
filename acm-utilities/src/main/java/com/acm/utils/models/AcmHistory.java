/**
 * Copyright(C)TALYS™-All Rights Reserved Unauthorized copying of this file,via any
 * medium/is*strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link ACM_HISTORY} class.
 *
 * @author hchaouachi
 * @since 0.1.0
 */

@Entity
@Table(name = "ACM_HISTORY")
public class AcmHistory {
	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_HISTORY", unique = true, nullable = false)
	private Long id;

	/** The id object. */
	@Column(name = "ID_OBJECT")
	private Long idObject;

	/** The actions. */
	@Column(name = "ACTIONS")
	private String actions;

	/** The value object. */
	@Column(name = "VALUE_OBJECT", length = 256)
	private String valueObject;

	/** The date update. */
	@Column(name = "DATE_ACTION")
	private Date dateAction;

	/** The action by. */
	@Column(name = "ACTION_BY", length = 256)
	private String actionBy;

	/** The type object. */
	@Column(name = "TYPE_OBJECT", length = 256)
	private String typeObject;

	/**
	 * Instantiates a new acm history.
	 */
	public AcmHistory() {

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
	 * Gets the actions.
	 *
	 * @return the actions
	 */
	public String getActions() {

		return actions;
	}

	/**
	 * Sets the actions.
	 *
	 * @param actions the new actions
	 */
	public void setActions(String actions) {

		this.actions = actions;
	}

	/**
	 * Gets the id object.
	 *
	 * @return the id object
	 */
	public Long getIdObject() {

		return idObject;
	}

	/**
	 * Sets the id object.
	 *
	 * @param idObject the new id object
	 */
	public void setIdObject(Long idObject) {

		this.idObject = idObject;
	}

	/**
	 * Gets the value object.
	 *
	 * @return the value object
	 */
	public String getValueObject() {

		return valueObject;
	}

	/**
	 * Sets the value object.
	 *
	 * @param valueObject the new value object
	 */
	public void setValueObject(String valueObject) {

		this.valueObject = valueObject;
	}

	/**
	 * Gets the date action.
	 *
	 * @return the date action
	 */
	public Date getDateAction() {

		return dateAction;
	}

	/**
	 * Sets the date action.
	 *
	 * @param dateAction the new date action
	 */
	public void setDateAction(Date dateAction) {

		this.dateAction = dateAction;
	}

	/**
	 * Gets the action by.
	 *
	 * @return the action by
	 */
	public String getActionBy() {

		return actionBy;
	}

	/**
	 * Sets the action by.
	 *
	 * @param actionBy the new action by
	 */
	public void setActionBy(String actionBy) {

		this.actionBy = actionBy;
	}

	/**
	 * Gets the type object.
	 *
	 * @return the type object
	 */
	public String getTypeObject() {

		return typeObject;
	}

	/**
	 * Sets the type object.
	 *
	 * @param typeObject the new type object
	 */
	public void setTypeObject(String typeObject) {

		this.typeObject = typeObject;
	}

}

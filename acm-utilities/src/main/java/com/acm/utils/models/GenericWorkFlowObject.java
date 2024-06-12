package com.acm.utils.models;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * The Class GenericWorkFlowObject.
 */
@Entity
@Table(name = "ACM_GENERIC_WORKFLOW_OBJECT")
public class GenericWorkFlowObject extends GenericModel {

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_GENERIC_WORKFLOW_OBJECT", unique = true, nullable = false)
	private Long id;

	/** The name. */
	@Column(name = "NAME")
	private String name;

	/** The generic work flow objects. */
	@OneToMany(mappedBy = "genericWorkFlowObject")
	@JsonIgnore
	private Set<Item> genericWorkFlowObjects = new HashSet<>();

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
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the new name
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the generic work flow objects.
	 *
	 * @return the generic work flow objects
	 */
	public Set<Item> getGenericWorkFlowObjects() {

		return genericWorkFlowObjects;
	}

	/**
	 * Sets the generic work flow objects.
	 *
	 * @param genericWorkFlowObjects the new generic work flow objects
	 */
	public void setGenericWorkFlowObjects(Set<Item> genericWorkFlowObjects) {

		this.genericWorkFlowObjects = genericWorkFlowObjects;
	}

}

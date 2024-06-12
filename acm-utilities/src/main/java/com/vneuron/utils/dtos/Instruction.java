package com.vneuron.utils.dtos;

/**
 * The Class Instruction.
 */
public class Instruction {

	/** The id. */
	public Long id;

	/** The created on. */
	public String createdOn;

	/** The label. */
	public String label;

	/** The name. */
	public String name;

	/** The description. */
	public String description;

	/** The blocking. */
	public Boolean blocking;

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
	 * Gets the created on.
	 *
	 * @return the created on
	 */
	public String getCreatedOn() {

		return createdOn;
	}

	/**
	 * Sets the created on.
	 *
	 * @param createdOn the new created on
	 */
	public void setCreatedOn(String createdOn) {

		this.createdOn = createdOn;
	}

	/**
	 * Gets the label.
	 *
	 * @return the label
	 */
	public String getLabel() {

		return label;
	}

	/**
	 * Sets the label.
	 *
	 * @param label the new label
	 */
	public void setLabel(String label) {

		this.label = label;
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
	 * Gets the blocking.
	 *
	 * @return the blocking
	 */
	public Boolean getBlocking() {

		return blocking;
	}

	/**
	 * Sets the blocking.
	 *
	 * @param blocking the new blocking
	 */
	public void setBlocking(Boolean blocking) {

		this.blocking = blocking;
	}

}

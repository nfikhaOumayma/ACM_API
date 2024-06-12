package com.acm.utils.dtos;

import java.util.List;

/**
 * This class represents a Data Transfer Object (DTO) for a list of keys.
 */
public class KeysDTO {
	private List<KeyDTO> keys;

	/**
	 * Gets the list of keys.
	 *
	 * @return The list of keys.
	 */
	public List<KeyDTO> getKeys() {

		return keys;
	}

	/**
	 * Sets the list of keys.
	 *
	 * @param keys The list of keys to set.
	 */
	public void setKeys(List<KeyDTO> keys) {

		this.keys = keys;
	}

	/**
	 * Returns a string representation of the KeysDTO object.
	 *
	 * @return A string representation of the KeysDTO.
	 */
	@Override
	public String toString() {

		return "KeysDTO{" + "keys=" + keys + '}';
	}
}

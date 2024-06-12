/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_charge_off.dtos;

import com.acm.utils.dtos.GenericDTO;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ArrearsDaysDTO.
 */
public class ArrearsDaysDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 973138394023602697L;

	/** The 307669. */
	@JsonProperty("307669")
	public int _307669;

	/**
	 * Gets the 307669.
	 *
	 * @return the 307669
	 */
	public int get_307669() {

		return _307669;
	}

	/**
	 * Sets the 307669.
	 *
	 * @param _307669 the new 307669
	 */
	public void set_307669(int _307669) {

		this._307669 = _307669;
	}

}

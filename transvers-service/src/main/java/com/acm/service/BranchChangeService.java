/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.BranchChangeDTO;

/**
 * {@link BranchChangeService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface BranchChangeService {

	/**
	 * Find.
	 *
	 * @author mlamloum
	 * @param lastBranchChangeIdSynchronized the last branch change id synchronized
	 * @return the list
	 */
	List<BranchChangeDTO> find(String lastBranchChangeIdSynchronized);

}

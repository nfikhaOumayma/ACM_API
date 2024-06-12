/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

/**
 * {@link LoadDataAccountGLService} class.
 *
 * @author yesser.somai
 * @since 1.0.8
 */
public interface LoadDataAccountGLService {

	/**
	 * Find account gl list.
	 *
	 * @author yesser.somai
	 * @param branchId the branch id
	 * @return the list
	 */
	List<String> findAccountGlList(Long branchId);

}

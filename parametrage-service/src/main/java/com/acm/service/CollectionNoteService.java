/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CollectionNoteDTO;

/**
 * The Interface CollectionNoteService.
 */
public interface CollectionNoteService {

	/**
	 * Find.
	 *
	 * @param collectionNoteDTO the collection note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<CollectionNoteDTO> find(CollectionNoteDTO collectionNoteDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @param collectionNoteDTO the collection note DTO
	 * @return the collection note DTO
	 */
	CollectionNoteDTO save(CollectionNoteDTO collectionNoteDTO);

	/**
	 * Save.
	 *
	 * @param collectionNoteDTO the collection note DTO
	 * @param insertBy the insert by
	 * @return the collection note DTO
	 */
	CollectionNoteDTO save(CollectionNoteDTO collectionNoteDTO, String insertBy);

}

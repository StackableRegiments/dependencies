package com.metl.data

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import net.liftweb.util.Helpers._
import net.liftweb.common._

import com.metl.data._
import Privacy._

trait MeTLDataGenerators {

	val genPrivacy = for {
		p <- Gen.oneOf(Privacy.PRIVATE, Privacy.PUBLIC, Privacy.NOT_SET)
	} yield p

	val genPoint = for {
		x <- arbitrary[Double]
		y <- arbitrary[Double]
		pressure <- arbitrary[Double]
	} yield Point(x, y, pressure) 

	val genPointList = for {
		p <- Gen.containerOf1[List, Point](genPoint)
	} yield p

	val genIdList = for {
		i <- Gen.containerOf1[List, String](Gen.alphaStr)
	} yield i

	val validTimestamp = arbitrary[Long] suchThat (_ > 0)

	val genColor = for {
		r <- Gen.choose(0, 255)
		g <- Gen.choose(0, 255)
		b <- Gen.choose(0, 255)
		a <- Gen.choose(0, 255)
	} yield Color(a, r, g, b)

	val genInk = for {
		author <- Gen.alphaStr 
		timestamp <- validTimestamp
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
		points <- genPointList
		checksum <- arbitrary[Double]
		startingSum <- arbitrary[Double]
		color <- genColor 
		thickness <- arbitrary[Double]
		isHighlighter <- arbitrary[Boolean]
	} yield MeTLInk(ServerConfiguration.empty, author, timestamp, checksum, startingSum, points, color, thickness, isHighlighter, target, privacy, slide, identity)

	val genMoveDelta = for {
		author <- Gen.alphaStr 
		timestamp <- validTimestamp
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
		inkIds <- genIdList
		textIds <- genIdList
		imageIds <- genIdList
		xTrans <- arbitrary[Double]
		yTrans <- arbitrary[Double]
		xScale <- arbitrary[Double]
		yScale <- arbitrary[Double]
		newPrivacy <- genPrivacy
		isDeleted <- arbitrary[Boolean]
	} yield MeTLMoveDelta(ServerConfiguration.empty, author, timestamp, target, privacy, slide, identity, 
        inkIds, textIds, imageIds, xTrans, yTrans, xScale, yScale, newPrivacy, isDeleted)

	val genImage = for {
		author <- Gen.alphaStr 
		target <- Gen.alphaStr 
		timestamp <- validTimestamp
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
		tag <- Gen.alphaStr
		x <- arbitrary[Double]
		y <- arbitrary[Double]
		width <- arbitrary[Double]
		height <- arbitrary[Double]
		source <- Gen.alphaStr map { s => if (!s.isEmpty) Full(s) else Full("unknown") }
	} yield MeTLImage(ServerConfiguration.empty, author, timestamp, tag, source, Full(Array.empty[Byte]), Empty, width, height, x, y, target, privacy, slide, identity)

	val genText = for {
        author <- Gen.alphaStr
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
		timestamp <- validTimestamp
		tag <- Gen.alphaStr
        caret <- Gen.choose(0, 200)
		text <- Gen.alphaStr 
		style <- Gen.alphaStr 
		family <- Gen.alphaStr 
		weight <- Gen.alphaStr 
        size <- arbitrary[Double]
        decoration <- Gen.alphaStr
		color <- genColor 
		x <- arbitrary[Double]
		y <- arbitrary[Double]
		width <- arbitrary[Double]
		height <- arbitrary[Double]
	} yield MeTLText(ServerConfiguration.empty, author, timestamp, text, height, width, caret, x, y, tag, style, family, weight, size, decoration, identity, target, privacy,
      slide, color)

	val genDirtyInk = for {
        author <- Gen.alphaStr
		timestamp <- validTimestamp
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
	} yield MeTLDirtyInk(ServerConfiguration.empty, author, timestamp, target, privacy, slide, identity)

	val genDirtyText = for {
        author <- Gen.alphaStr
		timestamp <- validTimestamp
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
	} yield MeTLDirtyText(ServerConfiguration.empty, author, timestamp, target, privacy, slide, identity)

	val genDirtyImage = for {
        author <- Gen.alphaStr
		timestamp <- validTimestamp
		target <- Gen.alphaStr 
		privacy <- genPrivacy
		slide <- Gen.numStr 
		identity <- Gen.alphaStr 
	} yield MeTLDirtyImage(ServerConfiguration.empty, author, timestamp, target, privacy, slide, identity)

    val genCommand = for {
        author <- Gen.alphaStr
        timestamp <- validTimestamp
        command <- Gen.alphaStr
        commandParams <- Gen.containerOf1[List, String](Gen.alphaStr)
    } yield MeTLCommand(ServerConfiguration.empty, author, timestamp, command, commandParams)

    val genSubmission = for {
        author <- Gen.alphaStr
        timestamp <- validTimestamp
        title <- Gen.alphaStr
        slideJid <- arbitrary[Int] 
        url <- Gen.alphaStr
    } yield MeTLSubmission(ServerConfiguration.empty, author, timestamp, title, slideJid, url) 

    val genQuiz = for {
        author <- Gen.alphaStr
        timestamp <- validTimestamp
        created <- arbitrary[Long]
        question <- Gen.alphaStr
        id <- Gen.numStr
        isDeleted <- arbitrary[Boolean]
        url <- Gen.alphaStr
        options <- Gen.containerOf1[List, QuizOption](genQuizOption)
    } yield MeTLQuiz(ServerConfiguration.empty, author, timestamp, created, question, id, Full(url), Empty, isDeleted, options)

    val genQuizOption = for {
        name <- Gen.alphaStr
        text <- Gen.alphaStr
    } yield QuizOption(name, text)
}

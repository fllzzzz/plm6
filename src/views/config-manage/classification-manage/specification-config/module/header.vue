<template>
  <div class="head-container">
    <el-tag v-if="crud.data.length" v-show="currentNode.name" class="filter-item" effect="plain" size="medium">{{
      `${currentNode.serialNumber} ${currentNode.name}`
    }}</el-tag>
    <el-tag v-else class="filter-item" type="info" size="medium">当前科目尚未配置规格</el-tag>
    <common-button
      v-permission="permission.add"
      class="filter-item"
      size="mini"
      type="primary"
      icon="el-icon-plus"
      @click.stop="crud.toAdd"
    >
      新增规格
    </common-button>
  </div>
</template>

<script setup>
import { inject } from 'vue'

import { regHeader } from '@compos/use-crud'

const defaultQuery = {
  id: undefined
}

const currentNode = inject('currentNode')
const permission = inject('permission')

const { crud } = regHeader(defaultQuery)
</script>

<style lang="scss" scoped>
.head-container {
  height: 42px;
  margin-left: 20px;
}
</style>

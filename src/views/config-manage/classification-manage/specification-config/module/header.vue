<template>
  <div class="head-container">
    <template v-if="currentNode.basicClass === classificationEnum.SECTION_STEEL.V">
      <el-tag class="filter-item" type="warning" size="medium" >“型材”请前往【型材库页面】编辑规格</el-tag>
    </template>
    <template v-else-if="crud.data.length">
      <el-tag v-show="currentNode.name" class="filter-item" effect="plain" size="medium">{{ `${currentNode.serialNumber} ${currentNode.name}` }}</el-tag>
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
    </template>
    <template v-else>
      <el-tag class="filter-item" type="info" size="medium">当前科目尚未配置规格</el-tag>
    </template>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import { classificationEnum } from '@enum-ms/classification'

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

<template>
  <div class="head-container">
    <template v-if="crud.data.length">
      <el-tag v-show="currentNode.name" effect="plain" size="medium">{{ `${currentNode.serialNumber} ${currentNode.name}` }}</el-tag>
      <common-button
        v-permission="permission.add"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click.stop="crud.toAdd"
      >
      新增规格
      </common-button>
    </template>
    <template v-else>
      <el-tag type="info" style="width: 160px" size="medium">当前科目尚未配置规格</el-tag>
    </template>
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

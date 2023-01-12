<template>
  <div style="display: flex">
    <summary-list style="width: 40%" ref="summaryListRef" @setInfo="setFInfo" />
    <div style="border-right: 1px solid #ededed; height: calc(100vh - 100px)"></div>
    <div class="app-container" v-show="isBlank(fInfo)">
      <div class="my-code">点击左表操作</div>
    </div>
    <detail-list v-show="!isBlank(fInfo)" ref="detailListRef" style="width: 59%" :fQuery="fQuery" :fInfo="fInfo" />
  </div>
</template>

<script setup>
import { computed, provide, ref, inject, watch } from 'vue'

import { mesWageAdjustPM as permission } from '@/page-permission/mes'
import { mapGetters } from '@/store/lib'
import { isBlank } from '@data-type/index'

import summaryList from './summary-list'
import detailList from './detail-list'

const commonPermission = inject('permission', permission)
provide('permission', commonPermission)

const summaryListRef = ref()
const detailListRef = ref()

const fQuery = computed(() => {
  return summaryListRef?.value?.query
})
const { globalProjectId } = mapGetters('globalProjectId')
provide('projectId', globalProjectId)

const fInfo = ref()

watch(
  [() => fQuery.value?.taskTypeEnum, () => globalProjectId.value],
  () => {
    fInfo.value = undefined
  }
)

function setFInfo(info) {
  fInfo.value = info
  detailListRef.value?.fetchProcess(info)
}
</script>

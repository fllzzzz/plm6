<template>
  <div class="head-container">
    <el-date-picker
      v-model="year"
      type="year"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择年份"
      style="width: 100px"
      class="filter-item"
      @change="fetchProjectList"
    />
    <el-input
      v-model.trim="filterText"
      size="small"
      placeholder="输入项目/单体/区域搜索"
      style="width: 200px"
      class="filter-item"
      clearable
    />
  </div>
  <div :style="{ height: `${maxHeight}px` }">
    <project-to-area-tree :tree-data="treeData" :filter-text="filterText" :loading="loading" v-bind="$attrs" />
  </div>
</template>

<script setup>
import { getProjectInfo } from '@/api/mes/work-order-manage/artifact.js'
import { ref, defineProps, watch, inject } from 'vue'
import moment from 'moment'

import checkPermission from '@/utils/system/check-permission'
import { artifactWorkOrderPM as permission } from '@/page-permission/mes'
import ProjectToAreaTree from '../../components/project-to-area-tree'

const crud = inject('crud')
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const loading = ref(false)

const treeData = ref([])
const year = ref(moment().valueOf().toString())
const filterText = ref('')

watch([() => year.value], () => {
  crud.data = []
  crud.query.projectId = undefined
})

fetchProjectList()

async function fetchProjectList() {
  if (!checkPermission(permission.get)) return
  try {
    filterText.value = ''
    loading.value = true
    const content = await getProjectInfo({
      localDateTime: year.value ? year.value : moment().valueOf().toString()
    })
    treeData.value = content
  } catch (error) {
    console.log('获取构件项目列表树木失败', error)
  } finally {
    loading.value = false
  }
}
</script>

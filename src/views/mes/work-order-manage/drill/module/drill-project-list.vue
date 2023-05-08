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
import { getProjectInfo } from '@/api/mes/work-order-manage/drill.js'
import { ref, defineProps, watch, inject } from 'vue'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { drillWorkOrderPM as permission } from '@/page-permission/mes'
import ProjectToAreaTree from '../../components/project-to-area-tree'

const crud = inject('crud')
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const year = ref(moment().valueOf().toString())
const filterText = ref('')
const treeData = ref([])
const loading = ref(false)

watch([() => year.value], () => {
  crud.data = []
  crud.query.projectId = undefined
})

fetchProjectList()

async function fetchProjectList() {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    treeData.value = []
    const data = await getProjectInfo({
      localDateTime: year.value ? year.value : moment().valueOf().toString(),
      processType: mesMachinePartOrderTypeEnum.DRILL_ORDER.V
    })
    treeData.value = data || []
  } catch (error) {
    console.log('获取构件项目列表错误', error)
  } finally {
    loading.value = false
  }
}
</script>

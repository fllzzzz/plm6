<template>
  <common-drawer ref="drawerRef" title="产线质检不合格列表" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag size="medium" effect="plain">
        生产线：<span>{{ info.productionLineName }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table
        v-permission="permission.print"
        api-key="mesQHSEProductionLineReport"
        :params="{ ...queryParams }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight"
        :data-format="columnsDataFormat"
        row-key="rowId"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="150">
          <template #default="{ row }">
            <span>{{ row.project }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="artifactSerialNumber" label="编号" align="center">
          <template #default="{ row }">
            <span>{{ row.artifactSerialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="qualityType" label="问题" align="center">
          <template #default="{ row }">
            <span>{{ row.qualityType }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="productUserName" label="制造人">
          <template #default="{ row }">
            <span>{{ row.productUserName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="InspectionUserName" label="检验人">
          <template #default="{ row }">
            <span>{{ row.InspectionUserName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="processName" label="工序" width="120">
          <template #default="{ row }">
            <span>{{ row.processName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量" width="100">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="createTime" label="日期" width="120">
          <template #default="{ row }">
            <span>{{ row.createTime }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/QHSE-manage/production-line-report'
import { defineProps, defineEmits, ref, inject, computed } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object
  },
  query: {
    type: Object
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

const permission = inject('permission')
// 表格列格式化
const columnsDataFormat = ref([
  ['project', 'parse-project'],
  ['createTime', ['parse-time', '{y}-{m}-{d}']]
])

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableLoading = ref(false)
const list = ref([])
const queryParams = computed(() => {
  return {
    productionLineId: props.info.productionLineId,
    startDate: props.query.startDate,
    endDate: props.query.endDate
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const content = await detail({
      ...queryParams.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
  } catch (error) {
    console.log('获取质检报表详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

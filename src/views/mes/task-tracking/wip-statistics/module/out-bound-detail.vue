<template>
  <common-drawer ref="drawerRef" title="钢材出库记录" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        项目：<span>{{ info.project?.serialNumber }}-{{ info.project?.name }}</span>
      </el-tag>
    </template>
    <template #content>
      <div class="head-drawer" style="display: flex; justify-content: space-between; margin-bottom: 8px">
        <common-radio-button
          v-model="type"
          :options="steelOutBoundRecordTypeEnum.ENUM"
          class="filter-item"
          type="enum"
          @change="fetchList"
        />
        <div style="width: 300px">
          <print-table api-key="mesOutBoundStatisticsList" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
        </div>
      </div>
      <common-table v-loading="tableLoading" :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="basicClass" :show-overflow-tooltip="true" label="物料种类" width="150" />
        <el-table-column :show-overflow-tooltip="true" key="specMerge" prop="specMerge" label="规格" align="center">
          <template v-slot="scope">
            <span>{{scope.row.specMerge }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column> -->
        <el-table-column :show-overflow-tooltip="true" align="center" prop="measureUnit" label="计量单位">
          <template #default="{ row }">
            <span>{{ row.measureUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="accountingUnit" label="核算单位" width="120">
          <template #default="{ row }">
            <span>{{ row.accountingUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="mete" label="核算量" width="100">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="outboundTime" label="出库日期" width="120">
          <template #default="{ row }">
            <span>{{ parseTime(row.outboundTime, '{y}-{m}-{d}') }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getOutbound } from '@/api/mes/task-tracking/wip-statistics.js'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { steelOutBoundRecordTypeEnum } from '@enum-ms/mes'
import { setSpecInfoToList } from '@/utils/wms/spec'
// import { specFormat } from '@/utils/wms/spec-format'
import { matClsEnum } from '@enum-ms/classification'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const type = ref(steelOutBoundRecordTypeEnum.OUTBOUND.V)
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

const dataFormat = ref([
  ['basicClass', ['parse-enum', matClsEnum, { bit: true }]]
])
// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.head-drawer'],
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
    projectId: props.info.project?.id,
    type: type.value
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content } = await getOutbound({
      ...queryParams.value
    })
    list.value = content.map(v => {
      return v
    })
    await setSpecInfoToList(list.value)
  } catch (error) {
    console.log('获取出库记录详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

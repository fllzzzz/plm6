<template>
  <common-drawer
    ref="drawerRef"
    :title="info.handleType && handleMethodEnumV[info.handleType].L+'详情'"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span>
          可操作{{ info.handleType && handleMethodEnumV[info.handleType].L }}的构件数量总和为{{
            info.canHandleTotalMete
          }}，请谨慎操作！</span
        >
      </div>
      <common-table ref="tableRef" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="构件编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="productionLineName" :show-overflow-tooltip="true" label="生产线">
          <template v-slot="scope">
            <span>{{ scope.row.productionLineName }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column prop="teamName" :show-overflow-tooltip="true" label="班组">
          <template v-slot="scope">
            <span>{{ scope.row.teamName }}</span>
          </template>
        </el-table-column> -->
        <template v-for="item in handleMethodEnumV[info.handleType].COLUMNS || []" :key="item.field">
          <el-table-column :prop="item.field" :show-overflow-tooltip="true" :label="item.label" :width="item.width" :align="item.align">
            <template v-slot="scope">
              <span>{{ scope.row[item.field] }}</span>
            </template>
          </el-table-column>
        </template>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { exceptionList, extraTaskList } from '@/api/mes/changed-manage/artifact'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { abnormalReportTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const tableRef = ref()
const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

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
const handleMethodEnumV = inject('handleMethodEnumV')
const handleMethodEnum = inject('handleMethodEnum')
const tableLoading = ref(false)
const list = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    } else {
      init()
    }
  },
  { immediate: true }
)

function init() {
  list.value = []
}

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    if (props.info.handleType === handleMethodEnum.DECREASE_TASK.V) {
      const { content } = await extraTaskList()
      _list = content
    } else {
      const { reportList } = await exceptionList({
        productType: props.info.productType,
        productId: props.info.productId
      })
      _list = reportList.map(v => {
        v.reportTypeText = abnormalReportTypeEnum.VL[v.reportType]
        return v
      })
    }
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.tip {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  color: red;
  font-size: 13px;
  margin-bottom: 15px;
  line-height: 20px;
  > span {
    display: inline-block;
  }
  > span:nth-child(1) {
    width: 50px;
    flex-shrink: 0;
  }
}
</style>

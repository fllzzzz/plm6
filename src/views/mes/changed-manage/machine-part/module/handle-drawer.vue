<template>
  <common-drawer ref="drawerRef" title="处理列表" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="40%">
    <template #titleRight>
      <common-button size="mini" type="primary" :loading="submitLoading" @click="submit">提交</common-button>
      <common-button size="mini" type="warning" @click="reset">重置</common-button>
    </template>
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span> 可操作的零件数量总和为{{ info.canHandleTotalMete }}，请谨慎操作！</span>
      </div>
      <common-table ref="tableRef" v-loading="tableLoading" :data="canHandleList" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="零件编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="lineName" :show-overflow-tooltip="true" label="生产线">
          <template v-slot="scope">
            <span>{{ scope.row.lineName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="teamName" :show-overflow-tooltip="true" label="班组">
          <template v-slot="scope">
            <span>{{ scope.row.teamName }}</span>
          </template>
        </el-table-column>
        <el-table-column label="操作" width="80" align="center">
          <template v-slot="scope">
            <common-button
              type="primary"
              size="mini"
              icon="el-icon-arrow-right"
              :disabled="remainHandleMete <= 0"
              @click="addRow(scope.$index, scope.row)"
            />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { exceptionList, extraTaskList, change } from '@/api/mes/changed-manage/artifact'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'
import { ElNotification } from 'element-plus'

import { deepClone } from '@data-type/index'

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
const tableLoading = ref(false)
const submitLoading = ref(false)
const originList = ref([])
const canHandleList = ref([])
const previewList = ref([])

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

const alreadyHandleMete = computed(() => {
  return previewList.value.reduce((pre, curr) => {
    if (curr.dealMete) {
      return pre + curr.dealMete
    } else {
      return pre
    }
  }, 0)
})

const remainHandleMete = computed(() => {
  return props.info.canHandleTotalMete - alreadyHandleMete.value
})

function init() {
  canHandleList.value = []
  previewList.value = []
}

function reset() {
  canHandleList.value = deepClone(originList.value)
  previewList.value = []
}

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await extraTaskList()
    _list = content.map((v) => {
      v.canHandleMete = v.extraQuantity
      return v
    })
  } catch (error) {
    console.log('获取处理列表失败', error)
  } finally {
    originList.value = deepClone(_list)
    canHandleList.value = deepClone(_list)
    tableLoading.value = false
  }
}

function addRow(index, row) {
  const _quantity = row.canHandleMete || 0
  if (_quantity > remainHandleMete.value) {
    row.dealMete = remainHandleMete.value
  } else {
    row.dealMete = _quantity
  }
  previewList.value.push({ ...row })
  canHandleList.value.splice(index, 1)
}

async function submit() {
  try {
    submitLoading.value = true
    const data = {}
    await change(data)
    ElNotification({
      title: '零件变更处理成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
  } catch (error) {
    console.log('变更处理失败', error)
  } finally {
    submitLoading.value = false
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

.list-content {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: flex-start;

  .handle-content {
    box-sizing: border-box;
    padding-right: 20px;
    flex: auto;
    min-width: 260px;
    width: calc(100% - 980px);
  }
  .preview-content {
    flex: none;
    width: 980px;
  }
}

.handle-title {
  padding: 5px 0px;
  box-sizing: border-box;
  font-weight: bold;
  color: #303133;
  display: flex;
  justify-content: space-between;
  align-items: center;
}
</style>

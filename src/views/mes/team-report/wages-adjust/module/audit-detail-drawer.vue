<template>
  <common-drawer ref="drawerRef" title="班组工价审核详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight>
      <template v-if="resultStatus & reviewStatusEnum.UNREVIEWED.V">
        <common-button
          size="mini"
          type="success"
          :auditLoading="auditLoading && checkStatus & reviewStatusEnum.PASS.V"
          @click="auditIt(reviewStatusEnum.PASS.V)"
        >
          通过
        </common-button>
        <common-button
          size="mini"
          type="danger"
          :auditLoading="auditLoading && checkStatus & reviewStatusEnum.REFUSE.V"
          @click="auditIt(reviewStatusEnum.REFUSE.V)"
        >
          拒绝
        </common-button>
      </template>
    </template>
    <template #content>
      <common-table row-key="rowId" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showFactory showWorkshop showProductionLine showTeam />
        <el-table-column prop="processName" show-overflow-tooltip label="涉及工序">
          <template #default="{ row }">
            <span>{{ row.processName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="oldPrice" show-overflow-tooltip label="内部定额">
          <template #default="{ row }">
            <span>{{ row.oldPrice }}{{ row.showUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="newPrice" show-overflow-tooltip label="变更后">
          <template #default="{ row }">
            <span>{{ row.newPrice }}{{ row.showUnit }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { checkDetail, check } from '@/api/mes/team-report/wages-adjust'
import { defineProps, defineEmits, ref, watch } from 'vue'
import { ElNotification } from 'element-plus'

import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'

const drawerRef = ref()

const emit = defineEmits(['update:visible', 'success'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  itemInfo: {
    type: Object
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

const auditLoading = ref(false)
const checkStatus = ref()
const resultStatus = ref()
const tableLoading = ref(false)
const list = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      resultStatus.value = props.itemInfo?.auditStatus
      fetchList()
    } else {
      checkStatus.value = undefined
    }
  },
  { immediate: true }
)

async function fetchList() {
  if (!props.itemInfo?.id) return
  try {
    tableLoading.value = true
    const { content } = await checkDetail(props.itemInfo?.id)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.showUnit = useWageQuotaUnit({ wageQuotaType: v.wageQuotaType }).unit
      return v
    })
  } catch (error) {
    console.log('获取工价调整审核详情失败', error)
  } finally {
    tableLoading.value = false
  }
}

async function auditIt(status) {
  try {
    auditLoading.value = true
    checkStatus.value = status
    await check({
      id: props.itemInfo?.id,
      status: status
    })
    resultStatus.value = status
    ElNotification({ title: '审核成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('工价调整审核失败', error)
  } finally {
    auditLoading.value = false
  }
}
</script>

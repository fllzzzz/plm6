<template>
  <common-drawer ref="drawerRef" title="项目工价审核" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" show-overflow-tooltip label="单体">
          <template v-slot="scope">
            <span>{{ scope.row.monomer?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="productProcessName" show-overflow-tooltip label="名称">
          <template v-slot="scope">
            <span>{{ scope.row.productProcessName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="processName" show-overflow-tooltip label="工序">
          <template v-slot="scope">
            <span>{{ scope.row.processName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="oldPrice" show-overflow-tooltip label="定额单价" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.oldPrice }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="newPrice" show-overflow-tooltip label="调整后" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.newPrice }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="userName" show-overflow-tooltip label="操作人">
          <template v-slot="scope">
            <span>{{ scope.row.userName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="createTime" show-overflow-tooltip label="日期" align="center">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditUserName" show-overflow-tooltip label="审核人">
          <template v-slot="scope">
            <span v-empty-text>{{ scope.row.auditUserName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditTime" show-overflow-tooltip label="审核日期" align="center">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'" v-empty-text>{{ scope.row.auditTime }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditStatus" show-overflow-tooltip label="操作" width="170px" align="center">
          <template v-slot="scope">
            <span v-if="scope.row.auditStatus === reviewStatusEnum.UNREVIEWED.V">
              <common-button size="mini" type="success" :disabled="scope.row.auditLoading" @click="auditIt(scope.row, reviewStatusEnum.PASS.V)">同意</common-button>
              <common-button size="mini" type="danger" :disabled="scope.row.auditLoading" @click="auditIt(scope.row, reviewStatusEnum.REFUSE.V)">拒绝</common-button>
            </span>
            <el-tag v-else :type="reviewStatusEnum.V[scope.row.auditStatus].TAG">
              {{ reviewStatusEnum.VL[scope.row.auditStatus] }}
            </el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { checkList, check } from '@/api/mes/team-report/wages-adjust'
import { defineProps, defineEmits, inject, ref, watch } from 'vue'

import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
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
const query = inject('query')

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)
const tableLoading = ref(false)
const list = ref([])

async function fetchList() {
  try {
    tableLoading.value = true
    const { content } = await checkList(query)
    list.value = content.map((v) => {
      v.auditLoading = false
      return v
    })
  } catch (error) {
    console.log('获取工价调整审核列表失败', error)
  } finally {
    tableLoading.value = false
  }
}

async function auditIt(row, status) {
  try {
    row.auditLoading = true
    await check({
      id: row.id,
      status
    })
    fetchList()
    emit('refresh')
  } catch (error) {
    console.log('工价调整审核失败', error)
  } finally {
    row.auditLoading = false
  }
}
</script>

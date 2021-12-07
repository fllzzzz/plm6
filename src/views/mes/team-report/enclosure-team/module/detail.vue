<template>
  <common-drawer
    ref="drawerRef"
    :title="`生产线：${info.workshop?.name}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称" min-width="120px">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plate" prop="plate" :show-overflow-tooltip="true" label="板型" min-width="120px">
          <template v-slot="scope">
            <span>{{ scope.row.plate }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`单长\n(mm)`" align="center" min-width="80px">
          <template v-slot="scope">
            <span>{{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" label="数量" align="center" min-width="80px" />
        <el-table-column key="totalArea" prop="totalArea" :label="`总面积\n(㎡)`" align="center" min-width="80px">
          <template v-slot="scope">
            <span>{{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="totalLength"
          prop="totalLength"
          :show-overflow-tooltip="true"
          :label="`总长度\n(m)`"
          align="center"
          min-width="80px"
        >
          <template v-slot="scope">
            <span>{{ toFixed(scope.row.totalLength, DP.MES_ENCLOSURE_L__M) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="productTime" prop="productTime" :show-overflow-tooltip="true" label="生产日期" align="center" width="160px">
          <template v-slot="scope">
            <span v-parse-time>{{ scope.row.productTime }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/enclosure-team'
import { defineProps, defineEmits, ref, watch } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

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

watch(
  () => [props.visible, props.info],
  ([visible]) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

const tableLoading = ref(false)
const list = ref([])
async function fetchList() {
  try {
    tableLoading.value = true
    const { content } = await detail(props.info.id)
    list.value = content
  } catch (error) {
    console.log('获取围护班组详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

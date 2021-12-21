<template>
  <common-drawer
    ref="drawerRef"
    :title="`${info.workshop?.name}>${info.productionLine?.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        <span>累计生产量：</span>
        <span>{{query.startDate}} 件</span> /
        <span>{{query.endDate}} </span>
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="'{y}-{m}-{d}'">{{query.startDate}}</span> ~ <span v-parse-time="'{y}-{m}-{d}'">{{query.endDate}}</span>
      </el-tag>
    </template>
    <template #titleRight> </template>
    <template #content>
      <common-table ref="tableRef" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template #default="{ row }">
            <span v-parse-project="{ project: row.project }" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column key="monomer.name" prop="monomer.name" :show-overflow-tooltip="true" label="单体">
          <template v-slot="scope">
            <span>{{ scope.row.monomer.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/team-report/in-staff/allocation-system'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

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
const query = inject('query')

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const _query = Object.assign(
      {
        factoryId: props.info?.factory?.id,
        productionLineId: props.info?.productionLine?.id,
        workshopId: props.info?.workshop?.id
      },
      query
    )
    const { content } = await detail(_query)
    _list = content
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<template>
  <common-drawer ref="drawerRef" title="任务量详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <!-- <el-tag size="small" effect="plain">
        项目：<span>{{ info.project?.serialNumber }}-{{ info.project?.name }}</span>
      </el-tag> -->
      <el-input
        v-model.trim="monomerId"
        size="small"
        placeholder="输入单体搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="searchQuery"
      />
      <el-input
        v-model.trim="serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px; margin-left: 8px"
        class="filter-item"
        clearable
        @keyup.enter="searchQuery"
      />
      <common-button
        class="filter-item"
        size="mini"
        style="margin-left: 8px"
        type="success"
        icon="el-icon-search"
        @click.stop="searchQuery"
      >
        搜索
      </common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <div style="width: 300px">
        <print-table v-permission="permission.print" :api-key="apiKey" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
      </div>
    </template>
    <template #content>
      <common-table v-loading="tableLoading" returnSourceData :data="list" :max-height="maxHeight" :show-empty-symbol="false" row-key="rowId" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体">
          <template #default="{ row }">
            <span>{{ row.monomer? row.monomer?.name : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单重" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="totalNetWeight" label="总重" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getOutbound } from '@/api/mes/task-tracking/wip-statistics.js'
import { defineProps, defineEmits, ref, computed, inject } from 'vue'
// import { steelOutBoundRecordTypeEnum } from '@enum-ms/mes'
// import { setSpecInfoToList } from '@/utils/wms/spec'
// import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const monomerId = ref()
const serialNumber = ref()
// const type = ref(steelOutBoundRecordTypeEnum.OUTBOUND.V)
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

const permission = inject('permission')
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

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
    projectId: props.info.project?.id
    // type: type.value,
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content } = await getOutbound({
      ...queryParams.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    // await setSpecInfoToList(list.value)
  } catch (error) {
    console.log('获取任务量详情失败', error)
  } finally {
    tableLoading.value = false
  }
}

function searchQuery() {
  fetchList()
}

function resetQuery() {
  monomerId.value = undefined
  serialNumber.value = undefined
  fetchList()
}
</script>

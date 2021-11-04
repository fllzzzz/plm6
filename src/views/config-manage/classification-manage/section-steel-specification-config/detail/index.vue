<template>
  <common-drawer
    ref="drawerRef"
    :title="sectionSteel.name ? `${sectionSteel.name}规格列表` : '型材规格列表'"
    custom-class="section-steel-detail"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1000"
  >
    <template #content>
      <!--工具栏-->
      <mHeader />
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        style="width: 100%"
        :max-height="maxHeight"
        @selection-change="crud.selectionChangeHandler"
      >
        <el-table-column type="selection" width="55" align="center" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('specification')"
          key="specification"
          :show-overflow-tooltip="true"
          prop="specification"
          label="规格"
          align="center"
          width="150"
        />

        <template v-for="sd in standard" :key="sd.id">
          <el-table-column
            v-if="columns.visible(`${prefix}${sd.id}`)"
            :show-overflow-tooltip="true"
            :prop="`${prefix}${sd.id}`"
            :label="`${sd.name}\n理论重量(kg/m)`"
            align="center"
            min-width="120"
          >
            <template v-slot="scope">
              {{ scope.row[`${prefix}${sd.id}`] }}
            </template>
          </el-table-column>
        </template>
        <!--编辑与删除-->
        <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="100px" align="center" fixed="right">
          <template v-slot="scope">
            <udOperation :show-del="false" :data="scope.row" :permission="permission" />
          </template>
        </el-table-column>
      </common-table>
      <!-- 表单 -->
      <m-form />
      <m-batch-form />
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi from '@/api/config/classification-manage/section-steel-spec-config-detail'
import { defineProps, defineEmits, provide, inject, ref } from 'vue'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import mBatchForm from './module/batch-form'

const emit = defineEmits(['update:visible'])
const prefix = 'unitNet_'
provide('prefix', prefix)

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  }
})

const optShow = {
  batchAdd: true,
  add: true,
  edit: false,
  del: true,
  download: false
}

const permission = inject('permission')
const standard = inject('standard')
const sectionSteel = inject('sectionSteel')
const drawerRef = ref()
const tableRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '型材规格',
    formStore: true,
    formStoreKey: 'CONFIG_SECTION_STEEL_SPEC_DETAIL',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    dataPath: null
  },
  tableRef
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchSpecsList })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.section-steel-detail',
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

// 数据处理
CRUD.HOOK.handleRefresh = (crud, res) => {
  if (Array.isArray(res.data)) {
    res.data.forEach(item => {
      if (Array.isArray(item.standard)) {
        item.standard.forEach(sd => {
          item[`${prefix}${sd.id}`] = sd.unitNet
        })
      }
    })
  }
}

function fetchSpecsList() {
  if (sectionSteel.value.id) {
    crud.query.id = sectionSteel.value.id
    crud.toQuery()
  }
}
</script>

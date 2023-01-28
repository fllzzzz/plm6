<template>
  <common-drawer ref="drawerRef" title="钢材领用记录" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag effect="plain" size="medium"> 清单量：{{ info?.listMete }}kg</el-tag>
      <el-tag effect="plain" size="medium"> 累计领用量：{{ info?.useMete }}kg</el-tag>
    </template>
    <template #titleRight>
      <print-table
        v-permission="permission.useRecordPrint"
        api-key="mesMainMaterialTrackUseRecord"
        :params="queryParams"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60">
          <template #default="{ row, $index }">
            <!-- 是否甲供材料 -->
            <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" />
            <span>{{ $index + 1 }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="classifyName" :show-overflow-tooltip="true" label="名称" align="center" min-width="140" />
        <el-table-column :show-overflow-tooltip="true" label="规格" align="center" min-width="200">
          <template #default="{ row }">
            <el-tooltip :content="specTip(row)" placement="left">
              <span>{{ specFormat(row) }}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="90" />
        <el-table-column prop="mete" :show-overflow-tooltip="true" label="重量(kg)" align="center" width="100" />
        <el-table-column prop="reviewerName" :show-overflow-tooltip="true" label="办理人" align="center" width="100" />
        <el-table-column prop="recipient.name" :show-overflow-tooltip="true" label="领用人" align="center" min-width="120">
          <template #default="{ row }">
            <span>{{ row.workshop?.name ? row.workshop?.name + ' > ' : '' }}{{ row.recipient?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="outboundTime" :show-overflow-tooltip="true" label="日期" align="center" width="120px">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.outboundTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { useDetail } from '@/api/mes/production-manage/dashboard/main-material-track'
import { defineProps, defineEmits, ref, computed } from 'vue'

import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import { mainMaterialTrackPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: Number
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerRef
)

const tableLoading = ref(false)
const list = ref([])
const queryParams = computed(() => {
  return {
    projectId: props.projectId,
    basicId: props.info?.basicId,
    configId: props.info?.configId,
    material: props.info?.material,
    thickness: props.info?.thickness
  }
})

async function fetchList() {
  try {
    tableLoading.value = true
    const { content, totalElements } = await useDetail({
      ...queryParams.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setSpecInfoToList(list.value)
    await numFmtByBasicClass(list.value, {
      toSmallest: false,
      toNum: false,
      accountingUnit: 'kg',
      accountingPrecision: 2
    })
  } catch (error) {
    console.log('获取钢材领用记录失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

<template>
  <common-drawer ref="drawerRef" title="班组任务详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" :size="1000">
    <template #content>
      <common-table ref="tableRef" :data="teamDetailData" :max-height="maxHeight" style="width: 100%">
        <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="productionLine" label="产线>班组" align="center">
          <template #default="{ row }">
            <span>{{ row.workshop.name }}>{{ row.productionLine.name }}>{{ row.groupName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="任务数" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" label="完成数" align="center"></el-table-column>
      </common-table>
      <!-- 分页 -->
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
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { getTeamDetail } from '@/api/bridge/bridge-production-manage/project-overview'
import { defineProps, defineEmits, ref } from 'vue'

const teamDetailData = ref([])
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  teamData: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: teamListGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: teamListGet })

async function teamListGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await getTeamDetail({
      monomerId: props.teamData.monomer?.id,
      areaId: props.teamData.area?.id,
      ...props.query,
      productId: props.teamData.id,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取班组任务详情失败', e)
  } finally {
    teamDetailData.value = _list
  }
}

const { maxHeight } = useMaxHeight({
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

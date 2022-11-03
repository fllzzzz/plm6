<template>
  <common-drawer ref="drawerRef" title="班组任务详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" :size="1000">
    <template #content>
      <common-table ref="tableRef" :data="teamDetailData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column prop="productionLine" label="产线>班组" align="center" >
            <template #default="{ row }">
                <span>{{ row.workshopName }}>{{ row.productionLineName }}>{{ row.groupName }}</span>
            </template>
        </el-table-column>
        <el-table-column prop="serialNumber" label="编号" align="center" ></el-table-column>
        <el-table-column prop="quantity" label="任务数" align="center" ></el-table-column>
        <el-table-column prop="completeQuantity" label="完成数" align="center" ></el-table-column>
      </common-table> 
      </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { getTeamDetail } from '@/api/mes/production-manage/dashboard/project-overview'
import { defineProps, defineEmits, ref, inject } from 'vue'

const teamDetailData = ref([])
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  teamData: {
    type: Object,
    default: () => {},
  },
  query: {
    type: Object
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: teamListGet })

const monomerId = inject('monomerId')
const areaId = inject('areaId')

async function teamListGet() {
    try {
        const data = await getTeamDetail({
            monomerId: monomerId.value,
            areaId: areaId.value,
            ...props.query,
            productId: props.teamData.id,
        })
        teamDetailData.value = data
    } catch (e) {
        console.log('获取班组任务详情失败', e);
    }
}

</script>
<style lang="scss" scoped>
</style>
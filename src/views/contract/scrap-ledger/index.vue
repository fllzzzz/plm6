<template>
  <div class="app-container">
    <mHeader @wayChange="wayChange" @dateChange="dateChange" @typeChange="typeChange" />
    <buy-query v-if="isTable === 1" :queryId="isTable" :date="date" :scrapType="scrapType" />
    <date-query v-else :queryId="isTable" :date="date" :scrapType="scrapType" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import mHeader from './module/header.vue'
import dateQuery from './module/date-query.vue'
import buyQuery from './module/buy-query.vue'
import moment from 'moment'

const isTable = ref(1)
const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])
const scrapType = ref(undefined)

const wayChange = (v) => {
  isTable.value = v
  date.value = [moment().startOf('month').valueOf(), moment().valueOf()]
  scrapType.value = undefined
}

const dateChange = (v) => {
  date.value = v
}

const typeChange = (v) => {
  scrapType.value = v
}

</script>
